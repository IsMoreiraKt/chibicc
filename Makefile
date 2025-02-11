# Compiler Flags
CFLAGS = -std=c11 -g -fno-common -Wall -Wno-switch

# Source and Object Files
SRCS = $(wildcard *.c)
OBJS = $(SRCS:.c=.o)

# Test Files
TEST_SRCS = $(wildcard test/*.c)
TESTS = $(TEST_SRCS:.c=.exe)

# Uncrustify Configuration
UNCRUSTIFY = uncrustify
UNCRUSTIFY_OPTS = -c linux.cfg

# Stage 1: Build the chibicc executable
chibicc: $(OBJS)
	$(CC) $(CFLAGS) -o $@ $^ $(LDFLAGS)

# Objects require chibicc.h
$(OBJS): chibicc.h

# Stage 1: Test Executables
test/%.exe: chibicc test/%.c
	./chibicc -Iinclude -Itest -c -o test/$*.o test/$*.c
	$(CC) -pthread -o $@ test/$*.o -xc test/common

# Run all tests for Stage 1
test: $(TESTS)
	for i in $^; do echo $$i; ./$$i || exit 1; echo; done
	test/driver.sh ./chibicc

# Run all tests for Stage 1 and Stage 2
test-all: test test-stage2

# Stage 2: Build the chibicc executable for Stage 2
stage2/chibicc: $(OBJS:%=stage2/%)
	$(CC) $(CFLAGS) -o $@ $^ $(LDFLAGS)

# Stage 2: Object Files
stage2/%.o: chibicc %.c
	mkdir -p stage2/test
	./chibicc -c -o $(@D)/$*.o $*.c

# Stage 2: Test Executables
stage2/test/%.exe: stage2/chibicc test/%.c
	mkdir -p stage2/test
	./stage2/chibicc -Iinclude -Itest -c -o stage2/test/$*.o test/$*.c
	$(CC) -pthread -o $@ stage2/test/$*.o -xc test/common

# Run all tests for Stage 2
test-stage2: $(TESTS:test/%=stage2/test/%)
	for i in $^; do echo $$i; ./$$i || exit 1; echo; done
	test/driver.sh ./stage2/chibicc

# Clean up build artifacts and temporary files
clean:
	rm -rf chibicc tmp* $(TESTS) test/*.s test/*.exe stage2
	find * -type f '(' -name '*~' -o -name '*.o' ')' -exec rm {} ';'

# Format the source code with Uncrustify
format: 
	$(UNCRUSTIFY) $(UNCRUSTIFY_OPTS) --no-backup $(wildcard *.c *.h)

# Declare non-file targets
.PHONY: test clean test-stage2 format
