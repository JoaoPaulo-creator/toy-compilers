# Makefile
.PHONY: build run test clean

# Build the compiler
build:
	go build -o toycompiler main.go

# Compile and run the test program
run: build
	./toycompiler test.toy
	nasm -f elf64 output.asm -o test.o
	# ld test.o -o test
	gcc -o run -no-pie test.o
	./run

# Test the compiler components
test:
	go test ./...

# Clean generated files
clean:
	rm -f toycompiler test.s test.o test

# Install dependencies (if any)
deps:
	go mod tidy

# Show help
help:
	@echo "Available targets:"
	@echo "  build  - Build the toy compiler"
	@echo "  run    - Compile test.toy and run the resulting executable"
	@echo "  test   - Run Go unit tests"
	@echo "  clean  - Remove generated files"
	@echo "  deps   - Download and tidy dependencies"
	@echo "  help   - Show this help message"
