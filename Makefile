# Compiler Flags
CFLAGS = -std=c11 -g -fno-common -Wall -Wno-switch

# Source and Object Files
SRCS = $(wildcard src/*.c)
OBJS = $(SRCS:src/%.c=build/%.o)

# Test Files
TEST_SRCS = $(wildcard test/*.c)
TESTS = $(TEST_SRCS:.c=.exe)

# Uncrustify Configuration
UNCRUSTIFY = uncrustify
UNCRUSTIFY_OPTS = -c linux.cfg

# Build directory
BUILD_DIR = build

# Ensure build directory exists before anything else
$(shell mkdir -p $(BUILD_DIR))

# Compile each .c file to .o in build directory
build/%.o: src/%.c
	$(CC) $(CFLAGS) -c $< -o $@

# Stage 1: Build the chibicc executable
chibicc: $(OBJS)
	$(CC) $(CFLAGS) -o $(BUILD_DIR)/$@ $^ $(LDFLAGS)

# Objects require chibicc.h
$(OBJS): src/chibicc.h

# Stage 1: Test Executables
test/%.exe: chibicc test/%.c
	./$(BUILD_DIR)/chibicc -Iinclude -Itest -c -o test/$*.o test/$*.c
	$(CC) -pthread -o $@ test/$*.o -xc test/common

# Run all tests for Stage 1
test: $(TESTS)
	for i in $^; do echo $$i; ./$$i || exit 1; echo; done
	test/driver.sh ./$(BUILD_DIR)/chibicc

# Run all tests for Stage 1 and Stage 2
test-all: test test-stage2

# Stage 2: Build the chibicc executable for Stage 2
stage2/chibicc: $(OBJS)
	$(CC) $(CFLAGS) -o $(BUILD_DIR)/chibicc $^ $(LDFLAGS)

# Stage 2: Object Files
stage2/%.o: chibicc %.c
	mkdir -p $(BUILD_DIR)/test
	./$(BUILD_DIR)/chibicc -c -o $(BUILD_DIR)/test/$*.o $*.c

# Stage 2: Test Executables
stage2/test/%.exe: stage2/chibicc test/%.c
	mkdir -p $(BUILD_DIR)/test
	./$(BUILD_DIR)/chibicc -Iinclude -Itest -c -o $(BUILD_DIR)/test/$*.o test/$*.c
	$(CC) -pthread -o $@ $(BUILD_DIR)/test/$*.o -xc test/common

# Run all tests for Stage 2
test-stage2: $(TESTS:test/%=stage2/test/%)
	for i in $^; do echo $$i; ./$$i || exit 1; echo; done
	test/driver.sh ./$(BUILD_DIR)/chibicc

# Clean up build artifacts and temporary files
clean:
	rm -rf $(BUILD_DIR) $(TESTS) test/*.s test/*.exe
	find * -type f '(' -name '*~' -o -name '*.o' ')' -exec rm {} ';'

# Format the source code with Uncrustify
format: 
	$(UNCRUSTIFY) $(UNCRUSTIFY_OPTS) --no-backup $(SRCS)

# Create build directory and move all compiled files there
build: $(OBJS) chibicc

# Declare non-file targets
.PHONY: test clean test-stage2 format build
