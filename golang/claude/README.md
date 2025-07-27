# Toy Compiler

A toy compiler for a C-like programming language written in Go. This compiler translates source code into x86_64 assembly using Intel syntax.

## Project Structure

```
toycompiler/
├── main.go              # Main entry point
├── lexer/
│   └── lexer.go         # Lexical analysis (tokenization)
├── ast/
│   └── ast.go           # Abstract Syntax Tree definitions
├── parser/
│   └── parser.go        # Syntax analysis (hand-written recursive descent parser)
├── codegen/
│   └── codegen.go       # Code generation (x86_64 assembly)
├── go.mod               # Go module definition
├── Makefile             # Build automation
├── test.toy             # Sample source file
└── README.md            # This file
```

## Language Features

### Data Types
- `int` - Integer numbers
- `bool` - Boolean values (`true`, `false`)
- Arrays - Dynamic arrays with `[]` syntax

### Control Structures
- `if`, `else if`, `else` statements
- `while` loops
- `for` loops

### Operations
- Arithmetic: `+`, `-`, `*`, `/`
- Comparison: `==`, `!=`, `<`, `>`, `<=`, `>=`
- Logical: `&&`, `||`, `!`
- Assignment: `=`

### Built-in Functions
- `print(value)` - Print a value followed by newline
- `array.length` - Get the length of an array (using dot notation)

### Array Operations
- Declaration: `int arr[] = [1, 2, 3];`
- Indexing: `arr[0]`
- Assignment: `arr[1] = 999;`
- Length: `arr.length`

## Compilation Phases

1. **Lexical Analysis** (`lexer/`) - Converts source code into tokens
2. **Syntax Analysis** (`parser/`) - Builds an Abstract Syntax Tree (AST)
3. **Code Generation** (`codegen/`) - Generates x86_64 assembly code

## Building and Running

### Prerequisites
- Go 1.21 or later
- GNU Assembler (`as`)
- GNU Linker (`ld`)
- Linux x86_64 system

### Quick Start

```bash
# Build the compiler
make build

# Compile and run the test program
make run

# Clean generated files
make clean
```

### Manual Steps

```bash
# Build the compiler
go build -o toycompiler main.go

# Compile a source file
./toycompiler test.toy

# Assemble and link
as --64 test.s -o test.o
ld test.o -o test

# Run the executable
./test
```

## Example Program

```c
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
}

func add(a, b) {
    return a + b;
}
```

## Architecture Details

### Lexer
- Hand-written lexical analyzer
- Supports all language tokens including keywords, operators, and literals
- Handles comments (`//` style)
- Provides error reporting with line and column information

### Parser
- Recursive descent parser (hand-written, no external tools)
- Builds a complete AST representation
- Supports operator precedence
- Handles all language constructs including nested statements

### Code Generator
- Generates Intel syntax x86_64 assembly
- Uses System V ABI calling convention
- Implements built-in functions (print, array.length)
- Supports local variables and arrays on the stack
- Generates optimized assembly for arithmetic and logical operations

### Memory Management
- Variables stored on the stack
- Arrays allocated contiguously on the stack
- Simple stack-based parameter passing for functions

## Testing

The compiler includes a comprehensive test program (`test.toy`) that demonstrates all language features:

- Variable declarations and assignments
- Arithmetic operations
- Boolean operations and conditions
- Control flow (if/else, while, for)
- Arrays and array operations
- Function calls and built-in functions

## Limitations

This is a toy compiler with some limitations:
- Limited to Linux x86_64
- No dynamic memory allocation
- Simple parameter passing (up to 2 parameters)
- Basic error reporting
- No optimization passes
- String literals have limited support

## Extending the Compiler

To add new features:

1. **New operators**: Add tokens to `lexer/lexer.go`, update parser precedence, implement in `codegen/codegen.go`
2. **New statements**: Add AST nodes in `ast/ast.go`, implement parsing in `parser/parser.go`, add code generation
3. **New built-ins**: Extend the call expression handling in `codegen/codegen.go`

## License

This is an educational project. Feel free to use and modify as needed.
