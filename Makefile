all: qpu-asm qpu-dis

qpu-asm: qpu-asm.cpp
	g++ -g -o qpu-asm qpu-asm.cpp

qpu-dis: qpu-dis.cpp
	g++ -g -o qpu-dis qpu-dis.cpp
