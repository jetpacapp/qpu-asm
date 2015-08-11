#include <iostream>
#include <stdlib.h>
#include <stdio.h>
#include <inttypes.h>
#include <map>
#include <vector>
#include <assert.h>
#include <errno.h>
#include <sstream>
#include <algorithm>
#include <unistd.h> // for getopt()

using namespace std;

enum token_t {
    END=-1,
    WORD,
    DOT,
    COMMA,
    SEMI,
    COLON,
};

struct QPUreg {
    enum { A, B, ACCUM, SMALL } file;
    int num;
};

struct relocation {
    string label;
    int pc;
};

struct context {
    const char *stream;
    map<string, int> labels;
    int pc;
    vector<relocation> relocations;
};


static string addOps[] = {
    "nop", "fadd", "fsub", "fmin", "fmax", "fminabs", "fmaxabs",
    "ftoi", "itof", "XXX", "XXX", "XXX", "add", "sub", "shr",
    "asr", "ror", "shl", "min", "max", "and", "or", "xor", "not",
    "clz", "XXX", "XXX", "XXX", "XXX", "XXX", "v8adds", "v8subs" };

static string mulOps[] = {
    "nop", "fmul", "mul24", "v8muld", "v8min", "v8max", "v8adds",
    "v8subs" };

static uint8_t addOpCode(const string& word)
{
    for (int i=0; i < 32; i++) {
        if (word == addOps[i])
            return i;
    }

    return 0xFF;
}

string printAddOpCode(uint8_t opcode) {
  assert((opcode >= 0) && (opcode < 32));
  return addOps[opcode];
}

static uint8_t mulOpCode(const string& word)
{
    for (int i=0; i < 8; i++) {
        if (word == mulOps[i])
            return i;
    }

    return 0xFF;
}

string printMulOpCode(uint8_t opcode) {
  assert((opcode >= 0) && (opcode < 8));
  return mulOps[opcode];
}

bool isRegisterWord(const string& word) { return word[0] == 'r'; }

string printRegister(const QPUreg& reg)
{
    char buffer[32];
    if (reg.file == QPUreg::A || reg.file == QPUreg::B) {
        snprintf(buffer, 32, "r%c%d", (reg.file == QPUreg::A) ? 'a' : 'b',
                                      reg.num);
    }
    else if (reg.file == QPUreg::ACCUM) {
        snprintf(buffer, 32, "r%d", reg.num);
    }
    else {
        snprintf(buffer, 32, ".0x%x.", reg.num);
    }

    return buffer;
}

void parsePossibleNumber(const char* possibleNumber, int base, int* outNumber, bool* outIsNumber) {
    char *endOfNumber;
    *outNumber = strtol(possibleNumber, &endOfNumber, base);
    *outIsNumber = (!(endOfNumber == possibleNumber || *endOfNumber != '\0' || errno == ERANGE));
}

bool parseRegister(const string& word, QPUreg& reg)
{
    if (word[0] != 'r')
        return false;

    int offset = 0;
    switch (word[1]) {
        case 'a': reg.file = QPUreg::A; offset = 2; break;
        case 'b': reg.file = QPUreg::B; offset = 2; break;
        default:
            reg.file = QPUreg::ACCUM;
            offset = 1;
    }

    const char* possibleNumber = (word.c_str() + offset);
    bool isNumber;
    int number;
    parsePossibleNumber(possibleNumber, 10, &number, &isNumber);
    if (!isNumber) {
      cerr << "Warning - couldn't interpret '" << word << "' as a register" << endl;
      return false;
    }
    reg.num = number;

    if ((reg.file == QPUreg::ACCUM) && (reg.num >= 6)) {
      fprintf(stderr, "Warning - accumulator out of range\n");
      return false;
    }

    return true;
}

bool parseFullImmediate(const string& str, uint32_t* outResult, uint32_t* outType)
{
    bool isNumber;
    if (str[0] == '[') {
      bool areAnyNegative = false;
      std:string cleanedString(str);
      cleanedString.erase(std::remove(cleanedString.begin(), cleanedString.end(), '['), cleanedString.end());
      cleanedString.erase(std::remove(cleanedString.begin(), cleanedString.end(), ']'), cleanedString.end());
      std::stringstream ss(cleanedString);
      std::string item;
      int itemCount = 0;
      int itemValues[16];
      while (std::getline(ss, item, ',')) {
        if (itemCount >= 16) {
          break;
        }
        bool isItemNumber;
        int itemValue;
        parsePossibleNumber(item.c_str(), 10, &itemValues[itemCount], &isItemNumber);
        if (!isItemNumber) {
          cerr << "Couldn't understand '" << item << "' as an entry in an immediate list" << endl;
          return false;
        }
        if (itemValues[itemCount] < 0) {
          areAnyNegative = true;
        }
        itemCount += 1;
      }

      if (itemCount < 16) {
          cerr << "Found too few items in the immediate array - expected 16 but had " << itemCount << endl;
          return false;
      }

      if (areAnyNegative) {
        *outType = 0x02;
      } else {
        *outType = 0x06;
      }

      uint32_t result = 0;
      for (int index = 0; index < 16; index += 1) {
        int value = itemValues[index];
        if (areAnyNegative) {
          if ((value < -1) || (value > 1)) {
            cerr << "Found an out-of-range signed value in the immediate array - expected -1, 0, or 1 but found " << value << endl;
            return false;
          }
        } else {
          if (value > 3) {
            cerr << "Found an out-of-range unsigned value in the immediate array - expected 0, 1, 2, or 3 but found " << value << endl;
            return false;
          }
        }
        uint32_t msb;
        uint32_t lsb;
        if (areAnyNegative) {
          msb = ((value & 0x80000000) >> 31);
          lsb = (value & 0x1);
        } else {
          msb = ((value & 0x2) >> 1);
          lsb = (value & 0x1);
        }
        result = (result | (lsb << (index + 0)));
        result = (result | (msb << (index + 16)));
      }

      *outResult = result;
      isNumber = true;
    } else {
      *outType = 0x00; // A full 32-bit immediate
      // if there is an 'x' we assume it's hex.
      if (str.find_first_of("x") != string::npos) {
          int signedResult;
          parsePossibleNumber(str.c_str(), 16, &signedResult, &isNumber);
          *outResult = signedResult;
      } else if (str.find_first_of(".f") != string::npos) {
          float f = strtof(str.c_str(), NULL);
          *outResult = *(uint32_t*)&f;
          isNumber = true;
      } else {
          int signedResult;
          parsePossibleNumber(str.c_str(), 10, &signedResult, &isNumber);
          *outResult = signedResult;
      }
    }
    return isNumber;
}

int32_t parseSmallImmediate(const string& str)
{
    int32_t result;
    if (str.find_first_of("x") != string::npos) {
        result = strtoul(str.c_str(), NULL, 16);
        if (result >= 16) {
          cerr << "Immediate out of range: " << str << endl;
          result = -1;
        }
    } else if (str.find_first_of("<<") != string::npos) {
        uint32_t shift = strtoul(str.c_str() + 2, NULL, 10);
        result = (48 + shift);
    } else if (str.find_first_of("-") != string::npos) {
        uint32_t value = strtoul(str.c_str() + 1, NULL, 10);
        if ((value < 1) || (value > 16)) {
          cerr << "Negative immediate out of range: " << str << endl;
          result = -1;
        } else {
          result = (32 + value);
        }
    } else {
        result = strtoul(str.c_str(), NULL, 10);
        if (result >= 16) {
          cerr << "Immediate out of range: " << str << endl;
          result = -1;
        }
    }
    return result;
}

uint8_t parseBranchCond(const string& str)
{
    if (str == "zf")            // all z flags set ("z full")
        return 0x0;
    if (str == "ze")            // all z flags clear ("z empty")
        return 0x1;
    if (str == "zs")            // any z flags set ("z set")
        return 0x2;
    if (str == "zc")            // any z flags clear ("z clear")
        return 0x3;
    if (str == "nf")            // all N flags set ("N full")
        return 0x4;
    if (str == "ne")            // all N flags clear ("N empty")
        return 0x5;
    if (str == "ns")            // any N flags set ("N set")
        return 0x6;
    if (str == "nc")            // any N flags clear ("N clear")
        return 0x7;
    if (str == "cf")            // all C flags set ("C full")
        return 0x8;
    if (str == "ce")            // all C flags clear ("C empty")
        return 0x9;
    if (str == "cs")            // any C flags set ("C set")
        return 0xa;
    if (str == "cc")            // any C flags clear ("C clear")
        return 0xb;
    if (str == "*")             // always
        return 0xf;

    // throw some exceptions
    cerr << "Invalid branch condition: " << str << endl;
    exit(0);
}

bool parsePacking(const string& str, uint32_t* outUnpack, uint32_t* outPM, uint32_t* outPack)
{
    *outUnpack = 0;
    *outPM = 0;
    *outPack = 0;
    if (str == "unpack32") {
        *outUnpack = 0;
    } else if (str == "unpack16a") {
        *outUnpack = 1;
    } else if (str == "unpack16b") {
        *outUnpack = 2;
    } else if (str == "unpack8ddupe") {
        *outUnpack = 3;
    } else if (str == "unpack8a") {
        *outUnpack = 4;
    } else if (str == "unpack8b") {
        *outUnpack = 5;
    } else if (str == "unpack8c") {
        *outUnpack = 6;
    } else if (str == "unpack8d") {
        *outUnpack = 7;
    } else if (str == "pack32") {
        *outPack = 0;
    } else if (str == "pack16a") {
        *outPack = 1;
    } else if (str == "pack16b") {
        *outPack = 2;
    } else if (str == "pack8ddupe") {
        *outPack = 3;
    } else if (str == "pack8a") {
        *outPack = 4;
    } else if (str == "pack8b") {
        *outPack = 5;
    } else if (str == "pack8c") {
        *outPack = 6;
    } else if (str == "pack8d") {
        *outPack = 7;
    } else if (str == "pack32clamp") {
        *outPack = 8;
    } else if (str == "pack16aclamp") {
        *outPack = 9;
    } else if (str == "pack16bclamp") {
        *outPack = 10;
    } else if (str == "pack8ddupeclamp") {
        *outPack = 11;
    } else if (str == "pack8aclamp") {
        *outPack = 12;
    } else if (str == "pack8bclamp") {
        *outPack = 13;
    } else if (str == "pack8cclamp") {
        *outPack = 14;
    } else if (str == "pack8dclamp") {
        *outPack = 15;
    } else {
      cerr << "Unknown pack condition: " << str << endl;
      return false;
    }

    return true;
}

uint8_t setALUMux(const QPUreg& reg)
{
    switch (reg.file) {
        case QPUreg::A: return 0x6;
        case QPUreg::B: return 0x7;
        case QPUreg::ACCUM:
            if (reg.num > 6 || reg.num < 0) {
                cerr << "Invalid accumulator register; out of range" << endl;
                exit(0);
            }
            return reg.num;
        case QPUreg::SMALL: return 0x7;
    }
}


token_t nextToken(const char *stream, string& out, const char **ptr)
{
    char buffer[128];
    int i = 0;

    *ptr = stream;
    if (!stream || !*stream)
        return END;

    while (*stream == ' ' || *stream == '\t')
        stream++;

    if (isdigit(*stream))
    {
        // read until we don't find a hex digit, x (for hex) or .
        while (isxdigit(*stream) || isdigit(*stream) || *stream == '.' || *stream == 'x') {
            buffer[i++] = *stream++;
            if (*stream == 0 || i > sizeof(buffer) - 1)
                break;
        }
        buffer[i++] = '\0';
        out = buffer;
        *ptr = stream;

        return WORD;
    }


    if (*stream == '.') { *ptr = stream+1; return DOT; }
    if (*stream == ',') { *ptr = stream+1; return COMMA; }
    if (*stream == ';') { *ptr = stream+1; return SEMI; }
    if (*stream == '#') { *ptr = stream+1; return END; }
    if (*stream == ':') { *ptr = stream+1; return COLON; }

    while (*stream != '.' && *stream != ',' && *stream != ';'
                          && *stream != ' ' && *stream != '\t'
                          && *stream != ':')
    {
        buffer[i++] = *stream++;
        if (*stream == 0 || i > sizeof(buffer)-1)
            break;
    }

    buffer[i++] = '\0';
    out = buffer;
    *ptr = stream;
    return WORD;
}


bool aluHelper(const char *stream, QPUreg& dest, QPUreg& r1, QPUreg& r2, uint8_t& sig, uint32_t& unpack, uint32_t& pm, uint32_t& pack, const char **ptr)
{
    string token_str;
    token_t tok = nextToken(stream, token_str, &stream);

    if (tok == DOT) {
        // conditional
        nextToken(stream, token_str, &stream);
        cout << "flag/conditional = " << token_str << endl;
        if (token_str == "ldtmu0") {
            sig = 10;
        } else if (token_str == "ldtmu1") {
            sig = 11;
        } else if (token_str == "tend") {
            sig = 3;
        } else if (parsePacking(token_str, &unpack, &pm, &pack)) {
          // Do nothing, the parse function has filled in the values
        } else {
          cout << "Conditional couldn't be understood: " << token_str << endl;
          return false;
        }
        tok = nextToken(stream, token_str, &stream);
    }

    // this is supposed to be the destination register
    if (tok != WORD) {
        cout << "Expecting word.  Got: " << token_str << endl;
        return false;
    }

    if (!parseRegister(token_str, dest)) {
      return false;
    }
    tok = nextToken(stream, token_str, &stream);
    if (tok != COMMA) return false;
    tok = nextToken(stream, token_str, &stream);
    if (!parseRegister(token_str, r1)) {
      return false;
    }

    tok = nextToken(stream, token_str, &stream);
    if (tok != COMMA) return false;
    tok = nextToken(stream, token_str, &stream);
    if (!parseRegister(token_str, r2)) {
        r2.file = QPUreg::SMALL;
        int32_t imm = parseSmallImmediate(token_str);
        if (imm < 0) {
          return false;
        }
        r2.num = imm;
    }

    /*
    cout << "dest: " << printRegister(dest) << ", r1: "
                     << printRegister(r1) << ", r2: "
                     << printRegister(r2) << endl;
                     */

    *ptr = stream;
    return true;
}


uint64_t assembleALU(context& ctx, string word)
{
    string token_str;
    uint8_t add_op = addOpCode(word);
    if (add_op == 0xFF) {
        cout << "FATAL (assert).  Bad ADD opcode: " << word << endl;
        return -1;
    }

    uint32_t unpack = 0;
    uint32_t pm = 0;
    uint32_t pack = 0;

    QPUreg addDest, addR1, addR2;
    QPUreg mulDest, mulR1, mulR2;

    uint8_t sig = 0x1;          // no-signal (TODO: plumb signals through)
    if (!aluHelper(ctx.stream, addDest, addR1, addR2, sig, unpack, pm, pack, &ctx.stream))
        return -1;

    token_t tok = nextToken(ctx.stream, token_str, &ctx.stream);
    // this should be a semi-colon
    tok = nextToken(ctx.stream, token_str, &ctx.stream);
    uint8_t mul_op = mulOpCode(token_str);
    if (mul_op == 0xFF) {
        cout << "FATAL (assert).  Bad MUL opcode: " << token_str << endl;
        return -1;
    }

    bool skipParseMul(false);
    if (mul_op == 0) {
        // nop.  If the next token is a semi or END, we'll generate
        // the registers for them
        const char *discard;
        tok = nextToken(ctx.stream, token_str, &discard);
        if (tok == END || tok == SEMI) {
            mulDest.num = 39;
            mulDest.file = (addDest.file == QPUreg::A) ? QPUreg::B : QPUreg::A;
            mulR1 = addR1;
            mulR2 = addR2;
            skipParseMul = true;
        }
    }

    if (!skipParseMul) {
        uint8_t junk;
        uint32_t junk32;
        if (!aluHelper(ctx.stream, mulDest, mulR1, mulR2, junk, junk32, junk32, junk32, &ctx.stream))
            return -1;
    }

    uint64_t ins = 0x0;
    uint8_t cond_add = 0x1;
    uint8_t cond_mul = 0x1;
    uint8_t sf = 0x1;
    if (add_op == 0)
        sf = 0x0;           // no set flags on nop

    // TODO: constraints.  We can only read from file A and file B once (dual-port)

    uint8_t ws = 0x0;
    // If the add pipe specifies file b for output, ws = 1
    if ((addDest.file == QPUreg::B) ||
        ((addDest.file == QPUreg::ACCUM) && (mulDest.file == QPUreg::A))) {
        ws = 0x1;
    }
    // if ws == 1, mul pipe must specify file a or accumulator for output
    if (ws == 0x1 && (mulDest.file != QPUreg::A) && (mulDest.file != QPUreg::ACCUM)) {
        cout << "constraint check failed.  mul pipe must specify register file A when write-swap set, but found " << printRegister(mulDest) << endl;
        return -1;
    }
    // if ws == 0, mul pipe must specify file b or accumulator for output
    if (ws == 0x0 && (mulDest.file != QPUreg::B) && (mulDest.file != QPUreg::ACCUM)) {
        cout << "constraint check failed.  mul pipe must specify register file B when write-swap clear, but found " << printRegister(mulDest) << endl;
        return -1;
    }

    // TODO: handle the accumulators and the small immediate
    uint8_t read_a = 0x0;
    uint8_t read_b = 0x0;
    bool isReadASet = false;
    bool isReadBSet = false;
    QPUreg candidates[] = {addR1, addR2, mulR1, mulR2};
    for (int index = 0; index < (sizeof(candidates)/sizeof(candidates[0])); index += 1) {
      QPUreg reg = candidates[index];
      if (reg.file == QPUreg::A) {
        if (isReadASet && (read_a != reg.num)) {
          fprintf(stderr, "Error: Can't set multiple different general registers as sources in a single ALU instruction\n");
          return -1;
        }
        isReadASet = true;
        read_a = reg.num;
      }
      if (reg.file == QPUreg::B) {
        if (isReadBSet && (read_b != reg.num)) {
          fprintf(stderr, "Error: Can't set multiple different general registers as sources in a single ALU instruction\n");
          return -1;
        }
        isReadBSet = true;
        read_b = reg.num;
      }
    }

    // checks:
    //   read_a not set and one of the muxes specifies file A ...
    //   same for read_b
    //   read_b set and there is a small immediate value

    // we could have immediates in the first register slot but not sure it makes sense
    // As above, we should check that read_b is not already set
    if (addR2.file == QPUreg::SMALL)    {
      if (isReadBSet && (read_b != addR2.num)) {
        fprintf(stderr, "Error: Can't set an immediate and general registers as sources in a single ALU instruction\n");
        return -1;
      }
      isReadBSet = true;
      read_b = addR2.num;
      sig = 13;
    }
    if (mulR2.file == QPUreg::SMALL)    {
      if (isReadBSet && (read_b != mulR2.num)) {
        fprintf(stderr, "Error: Can't set an immediate and general registers as sources in a single ALU instruction\n");
        return -1;
      }
      isReadBSet = true;
      read_b = mulR2.num;
      sig = 13;
    }

    // The accumulators are mapped to r32-35 when writing to them as destinations
    if (addDest.file == QPUreg::ACCUM) {
      addDest.num += 32;
    }
    if (mulDest.file == QPUreg::ACCUM) {
      mulDest.num += 32;
    }

    uint8_t add_a = setALUMux(addR1) & 0x7;
    uint8_t add_b = setALUMux(addR2) & 0x7;
    uint8_t mul_a = setALUMux(mulR1) & 0x7;
    uint8_t mul_b = setALUMux(mulR2) & 0x7;
    read_a &= 0x3f;
    read_b &= 0x3f;
    mul_op &= 0x7;
    add_op &= 0x1f;
    addDest.num &= 0x3f;
    mulDest.num &= 0x3f;
    cond_add &= 0x7;
    cond_mul &= 0x7;
    sf &= 0x1;
    ws &= 0x1;

//    printf("Assembling ALU instruction: %s, %d, %d\n", printRegister(addDest).c_str(), ws, sig);

    printf("ALU: %s %s, %s, %s; %s %s, %s, %s\n",
      printAddOpCode(add_op).c_str(),
      printRegister(addDest).c_str(),
      printRegister(addR1).c_str(),
      printRegister(addR2).c_str(),
      printMulOpCode(mul_op).c_str(),
      printRegister(mulDest).c_str(),
      printRegister(mulR1).c_str(),
      printRegister(mulR2).c_str()
    );

    ins = ((uint64_t)sig << 60) |
      ((uint64_t)unpack << 57) |
      ((uint64_t)pm << 56) |
      ((uint64_t)pack << 52) |
      ((uint64_t)cond_add << 49) |
      ((uint64_t)cond_mul << 46) |
      ((uint64_t)sf << 45) |
      ((uint64_t)ws << 44);
    ins |= ((uint64_t)addDest.num << 38) | ((uint64_t)mulDest.num << 32) | ((uint64_t)mul_op << 29) | ((uint64_t)add_op << 24);
    ins |= ((uint64_t)read_a << 18) | ((uint64_t)read_b << 12) | ((uint64_t)add_a << 9) | ((uint64_t)add_b << 6) | ((uint64_t)mul_a << 3) | mul_b;

    return ins;
}

uint64_t assembleLDI(context& ctx, string word)
{
    cout << "Assembling LDI instruction ... " << endl;

    string token_str;
    token_t tok = nextToken(ctx.stream, token_str, &ctx.stream);

    if (tok == DOT) {
        // conditional ... conditionals should be on each register ?
        cout << "conditional ... ";
        // chew the conditional
        nextToken(ctx.stream, token_str, &ctx.stream);

        tok = nextToken(ctx.stream, token_str, &ctx.stream);
    }

    // this is supposed to be the register
    if (tok != WORD) return -1;

    QPUreg register1, register2;
    // check errors here
    if (!parseRegister(token_str, register1)) {
      return false;
    }
    tok = nextToken(ctx.stream, token_str, &ctx.stream);
    if (tok != COMMA) return -1;
    tok = nextToken(ctx.stream, token_str, &ctx.stream);

    // this can either be another register
    // (in which case we'll use both ALUs to set)
    // or an immediate value (in which case we'll use rX39)
    register2.num = 39;
    register2.file = (register1.file == QPUreg::A) ? QPUreg::B : QPUreg::A;
    if (isRegisterWord(token_str)) {
        if (!parseRegister(token_str, register2)) {
          return -1;
        }
        tok = nextToken(ctx.stream, token_str, &ctx.stream);
        // check that this is a comma ...
    }

    uint32_t immediateType = 0x00; // A full 32-bit immediate
    unsigned int immediate;
    string restOfLine(ctx.stream);
    restOfLine = (token_str + restOfLine);
    if (!parseFullImmediate(restOfLine, &immediate, &immediateType)) {
      cerr << "Immediate couldn't be parsed: " << restOfLine << endl;
      return -1;
    }

    cout << "r1: " << printRegister(register1) << ", r2: "
                   << printRegister(register2) << ", immed: 0x"
                   << hex << immediate << dec << endl;

    // The accumulators are mapped to r32-35 in this context
    if (register1.file == QPUreg::ACCUM) {
      register1.num += 32;
    }
    if (register2.file == QPUreg::ACCUM) {
      register2.num += 32;
    }

    uint32_t high = (uint32_t)0xE << 28;
    high |= immediateType << 24;
    high |= (uint32_t)0x1 << 17;      // cond_add
    high |= (uint32_t)0x1 << 14;      // cond_mul
    high |= (uint32_t)0x0 << 13;      // sf
    high |= (uint32_t)0x0 << 12;      // ws
    uint8_t addreg = (register1.file != QPUreg::B) ? register1.num : register2.num;
    uint8_t mulreg = (register1.file == QPUreg::B) ? register1.num : register2.num;
    high |= (uint32_t)addreg << 6;
    high |= mulreg;
    uint64_t ins = ((uint64_t)high << 32) | immediate;

    return ins;
}

uint64_t assembleBRANCH(context& ctx, string word)
{
    cout << "Assembing BRANCH instruction" << endl;

    QPUreg dest;
    string token_str;
    token_t tok = nextToken(ctx.stream, token_str, &ctx.stream);

    // relative or absolute branch?
    uint8_t relative = 1;
    if (word == "bra")
        relative = 0;

    uint8_t branchCondition = 0xf;          // by default: always (unconditional branch)
    if (tok == DOT) {
        // conditional
        nextToken(ctx.stream, token_str, &ctx.stream);
        branchCondition = parseBranchCond(token_str);
        tok = nextToken(ctx.stream, token_str, &ctx.stream);
    }

    // this is the destination register
    if (tok != WORD) {
        cerr << "branch expecting destination register." << endl;
        return -1;
    }
    if (!parseRegister(token_str, dest)) {
      return false;
    }
    tok = nextToken(ctx.stream, token_str, &ctx.stream);
    if (tok != COMMA) return false;
    tok = nextToken(ctx.stream, token_str, &ctx.stream);
    if (tok != WORD) {
        cerr << "branch expecting label/target" << endl;
        return -1;
    }

    // look it up in the labels map
    int target = 0xFFFFFFFF;
    if (ctx.labels.count(token_str) < 1) {
        relocation r;
        r.label = token_str;
        r.pc = ctx.pc;
        ctx.relocations.push_back(r);
    } else
        target = ctx.labels[token_str];
    int offset = target - (ctx.pc+4*8);

    uint8_t raddr_a = 0;           // raddr_a is only 5-bits?
    uint8_t use_reg = 0;
    // if there's a third argument, it is a register offset
    const char *discard;
    tok = nextToken(ctx.stream, token_str, &discard);
    if (tok == COMMA) {
        QPUreg offsetReg;
        // chew the comma we just read
        ctx.stream = discard;
        tok = nextToken(ctx.stream, token_str, &ctx.stream);
        if (!parseRegister(token_str, offsetReg)) {
          return -1;
        }
        if (offsetReg.file != QPUreg::A) {
            cerr << "branch target offset register must be file A" << endl;
            return -1;
        }
        if (offsetReg.num > 31) {
            cerr << "branch target offset register must be < 32" << endl;
            return -1;
        }
        raddr_a = offsetReg.num;
        use_reg = 1;
    }

    uint8_t waddr_add = 39;         // link address appears at ALU outputs
    uint8_t waddr_mul = 39;
    if (dest.file == QPUreg::A) waddr_add = dest.num;
    if (dest.file == QPUreg::B) waddr_mul = dest.num;

    // TODO: generate absolute branches too

    uint64_t ins = (uint64_t)0xF << 60;
    ins |= (uint64_t)branchCondition << 52;
    ins |= (uint64_t)relative << 51;
    ins |= (uint64_t)use_reg << 50;
    ins |= (uint64_t)raddr_a << 45;
    ins |= (uint64_t)0x0 << 44;                       // write-swap
    ins |= (uint64_t)waddr_add << 38;
    ins |= (uint64_t)waddr_mul << 32;
    ins |= (uint32_t)offset;

    return ins;
}

uint64_t assembleSEMA(context& ctx, string word)
{

    uint64_t ins = (uint64_t)0x74 << 57;

    string token_str;
    token_t tok = nextToken(ctx.stream, token_str, &ctx.stream);
    if (tok != WORD) {
        cerr << "semaphore instruction expecting down/up or acquire/release" << endl;
        return -1;
    }

    uint8_t sa = 0;             // up
    if (token_str == "down" || token_str == "acquire")
        sa = 1;

    tok = nextToken(ctx.stream, token_str, &ctx.stream);
    if (tok != COMMA)   return -1;
    tok = nextToken(ctx.stream, token_str, &ctx.stream);
    uint32_t imm = parseSmallImmediate(token_str);
    if (imm < 0) {
        cerr << "semaphore out of range" << endl;
        return -1;
    }
    // cond_add, cond_mul = NEVER, ws, sf = false
    ins |= (uint64_t)39 << 38;          // waddr_add
    ins |= (uint64_t)39 << 32;          // waddr_mul
    ins |= sa << 4;
    ins |= (uint8_t)imm;

    cout << "Assembling SEMAPHORE instruction (" << imm << "), " << (int)sa << endl;

    return ins;
}


int main(int argc, char **argv)
{
    char *outfname = 0;
    int c;

    char* writeCPP = NULL;
    while ((c = getopt(argc, argv, "o:c:")) != -1) {
        switch (c) {
            case 'o':
                outfname = optarg;
                break;
            case 'c':
                writeCPP = optarg;
                break;
        }
    }

    if (!outfname) {
        cerr << "Usage: " << argv[0] << " -o <output>" << endl;
        return -1;
    }

    char line[128];
    string token_string;

    struct context ctx;
    ctx.pc = 0;

    vector<uint64_t> instructions;

    while (cin.getline(line, 128))
    {
        const char *p = line;
        ctx.stream = p;
        token_t tok = nextToken(ctx.stream, token_string, &ctx.stream);

        if (tok == END)
            continue;

        if (tok == WORD)
        {
            // read-ahead to see if the next token is a colon in which case
            // this is a label.
            const char *discard = NULL;
            string nextTokenStr;
            if (nextToken(ctx.stream, nextTokenStr, &discard) == COLON) {
                ctx.labels[token_string] = ctx.pc;
                continue;
            }

            enum { INVALID, ALU, BRANCH, LDI, SEMA } opType = INVALID;
            if (addOpCode(token_string) != 0xFF || mulOpCode(token_string) != 0xFF)
                opType = ALU;
            if (token_string == "ldi") opType = LDI;
            if (token_string == "bra" || token_string == "brr") opType = BRANCH;
            if (token_string == "sema") opType = SEMA;

            if (opType == INVALID) {
                cout << "Unable to assemble line; invalid opcode: " << line << endl;
                return -1;
            }

            uint64_t ins = 0;
            switch (opType) {
                case ALU: ins = assembleALU(ctx, token_string); break;
                case BRANCH: ins = assembleBRANCH(ctx, token_string); break;
                case LDI: ins = assembleLDI(ctx, token_string); break;
                case SEMA: ins = assembleSEMA(ctx, token_string); break;
            }

            if (ins == (uint64_t)-1) {
                cerr << "Error on line: " << line << endl;
                return -1;
            }

            instructions.push_back(ins);
            ctx.pc += 8;            // bytes;
        }
    }

    // Process relocations
    ctx.labels["ZERO"] = 0x0;
    for (int i=0; i < ctx.relocations.size(); i++)
    {
        relocation& r = ctx.relocations[i];
        if (ctx.labels.count(r.label) < 1)
        {
            cerr << "undefined label: " << r.label << endl;
            return -1;
        }
        int offset = ctx.labels[r.label] - (r.pc + 4*8);
        if (r.label == "ZERO")
            offset = 0x0;
        cout << "Processing relocation at " << r.pc << " : " << r.label
                                            << " : " << offset << endl;
        uint64_t ins = instructions[r.pc / 8];
        ins &= (uint64_t)0xFFFFFFFF << 32;   // zero bottom 32-bits for new value
        ins |= (uint32_t)offset;
        instructions[r.pc / 8] = ins;
    }

    FILE *outfile = fopen(outfname, "w");
    if (!outfile)
    {
        cerr << "Unable to open output file " << string(outfname) << endl;
        return -1;
    }

    if (writeCPP) {
      fprintf(outfile, "#include <stdint.h>\n");
      fprintf(outfile, "#include <stddef.h>\n\n");
      fprintf(outfile, "uint32_t %s[%d] = {\n", writeCPP, (instructions.size() * 2));
      uint32_t* instructionsData = (uint32_t*)(&instructions[0]);
      for (int i=0; i < instructions.size(); i++) {
        fprintf(outfile, "  0x%08x, 0x%08x,\n", instructionsData[(i * 2) + 0], instructionsData[(i * 2) + 1]);
      }
      fprintf(outfile, "};\n\n");
      fprintf(outfile, "size_t %sByteCount = %d;\n", writeCPP, (instructions.size() * 8));
    } else {
      for (int i=0; i < instructions.size(); i++)
          fwrite(&instructions[i], sizeof(uint64_t), 1, outfile);
    }

    fclose(outfile);
    cout << "Done.  Num instructions: " << instructions.size() << ", "
         << instructions.size() * 8 << " bytes." << endl;
}
