package main

import (
	"fmt"
	"os"
	"toy-compiler/backend"
	"toy-compiler/frontend"
)

func main() {
	if len(os.Args) != 2 {
		fmt.Println("Usage: toy-compiler <source-file>")
		os.Exit(1)
	}

	// Read source file
	sourceFile := os.Args[1]
	source, err := os.ReadFile(sourceFile)
	if err != nil {
		fmt.Printf("Error reading file: %v\n", err)
		os.Exit(1)
	}

	// Lexical analysis
	lexer := frontend.NewLexer(string(source))
	tokens, err := lexer.Scan()
	if err != nil {
		fmt.Printf("Lexical error: %v\n", err)
		os.Exit(1)
	}

	// Parsing
	parser := frontend.NewParser(tokens)
	ast, err := parser.Parse()
	if err != nil {
		fmt.Printf("Parse error: %v\n", err)
		os.Exit(1)
	}

	// Code generation
	codegen := backend.NewCodeGen(ast)
	assembly, err := codegen.Generate()
	if err != nil {
		fmt.Printf("Codegen error: %v\n", err)
		os.Exit(1)
	}

	// Output assembly to file
	outputFile := "output.asm"
	err = os.WriteFile(outputFile, []byte(assembly), 0644)
	if err != nil {
		fmt.Printf("Error writing output: %v\n", err)
		os.Exit(1)
	}

	fmt.Println("Compilation successful. Assembly output written to", outputFile)
}
