CXXFLAGS=-std=c++17 -static -g -Wall -Wextra -Werror
CXX=g++
SRCS=$(wildcard *.cpp)
OBJS=$(SRCS:.c=.o)
TEST_SRCS=$(wildcard tests/*.tc)
TESTS=$(TEST_SRCS:.tc=.exe)
RISCV=./riscv

all: clean tcc

tcc: $(OBJS)
	$(CXX) $(CXXFLAGS) -o $@ $^ $(LDFLAGS)

tests/%.exe: all tests/%.tc
	./tcc -opt > tests/$*.s < tests/$*.tc
	@${RISCV}/bin/riscv32-unknown-linux-gnu-gcc -static -o $@ tests/$*.s

test: $(TESTS)
	@for i in $^; do \
		echo $$i; \
		qemu-riscv32 -L ${RISCV}/sysroot ./$$i; \
		echo "RESULT: $$?"; \
		echo; \
	done

clean:
	@rm -rf *.s *.o tcc tmp* $(TESTS) tests/*.s tests/*.exe

.PHONY: test clean
