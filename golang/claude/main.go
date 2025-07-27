// main.go
package main

import (
	"fmt"
	"os"

	// "toycompiler/ast"
	"toycompiler/codegen"
	"toycompiler/lexer"
	"toycompiler/parser"
)

func main() {
	if len(os.Args) < 2 {
		fmt.Fprintf(os.Stderr, "Usage: %s <source-file>\n", os.Args[0])
		os.Exit(1)
	}

	sourceFile := os.Args[1]

	// Read source file
	source, err := os.ReadFile(sourceFile)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error reading file: %v\n", err)
		os.Exit(1)
	}

	fmt.Printf("Compiling %s...\n", sourceFile)

	// Phase 1: Lexical Analysis
	fmt.Println("Phase 1: Lexical Analysis")
	tokens, err := lexer.Tokenize(string(source))
	if err != nil {
		fmt.Fprintf(os.Stderr, "Lexer error: %v\n", err)
		os.Exit(1)
	}
	fmt.Printf("Generated %d tokens\n", len(tokens))

	// Phase 2: Parsing
	fmt.Println("Phase 2: Parsing")
	astRoot, err := parser.Parse(tokens)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Parser error: %v\n", err)
		os.Exit(1)
	}
	fmt.Println("AST created successfully")

	// Phase 3: Code Generation
	fmt.Println("Phase 3: Code Generation")
	assembly, err := codegen.Generate(astRoot)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Code generation error: %v\n", err)
		os.Exit(1)
	}

	// Write assembly to output file
	outputFile := sourceFile[:len(sourceFile)-4] + ".s"
	err = os.WriteFile(outputFile, []byte(assembly), 0644)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error writing output file: %v\n", err)
		os.Exit(1)
	}

	fmt.Printf("Assembly written to %s\n", outputFile)
	fmt.Println("Compilation completed successfully!")
}
