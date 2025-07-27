Toy Compiler
A toy compiler for a C-like programming language written in Go, targeting x86_64 assembly.
Project Structure

main.go: Entry point for the compiler.
frontend/lexer.go: Lexical analyzer.
frontend/parser.go: Hand-built parser producing an AST.
backend/codegen.go: Code generator for x86_64 assembly.

Features

Supports int, bool, and arrays.
Built-in functions: print and length (accessed via . syntax, e.g., arr.length).
Control structures: if, if-else, while, for.
Arithmetic operations: +, -, *, /.
Boolean operations: !, ==, <.
Array indexing: arr[0].
Function declarations and calls.

Usage

Compile the compiler:
go build -o toy-compiler main.go


Run the compiler on a source file:
./toy-compiler test.toy


Assemble and link the output (output.asm):
nasm -f elf64 output.asm
gcc -o program output.o


Run the program:
./program



Example Input
// test.toy
func main() {
    int x = 10;
    int y = 20;
    int result = x + y;
    print(result);

    bool flag = true;
    if (!flag) {
        print("Flag is false");
    } else {
        print("Flag is true");
    }

    int i = 0;
    while (i < 5) {
        print(i);
        i = i + 1;
    }

    int arr[] = [100, 200, 300];
    print(arr[0]);
    print(arr.length);
    arr[1] = 999;
    print(arr[1]);

    for (int j = 0; j < 3; j = j + 1) {
        print(j);
    }

    if (x == 10) {
        print("x is 10");
    } else if (y == 20) {
        print("y is 20");
    } else {
        print("neither");
    }
}

func add(a, b) {
    return a + b;
}

Notes

Uses Intel syntax for x86_64 assembly.
Arrays store length at base-8 offset.
Requires nasm and gcc for assembling and linking.
Error handling is basic; improvements can be made for production use.
