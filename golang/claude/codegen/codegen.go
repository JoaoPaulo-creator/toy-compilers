// codegen/codegen.go
package codegen

import (
	"fmt"
	"strings"

	"toycompiler/ast"
	"toycompiler/lexer"
)

// CodeGenerator represents the code generation phase
type CodeGenerator struct {
	output       strings.Builder
	labelCounter int
	stackOffset  int            // Current stack offset for local variables
	variables    map[string]int // Variable name to stack offset mapping
	arrays       map[string]int // Array name to size mapping
	currentFunc  string         // Current function being generated
}

// NewCodeGenerator creates a new code generator instance
func NewCodeGenerator() *CodeGenerator {
	return &CodeGenerator{
		variables: make(map[string]int),
		arrays:    make(map[string]int),
	}
}

// Generate performs code generation for the AST
func Generate(program *ast.Program) (string, error) {
	generator := NewCodeGenerator()
	return generator.generateProgram(program)
}

// generateProgram generates assembly for the entire program
func (cg *CodeGenerator) generateProgram(program *ast.Program) (string, error) {
	// Generate assembly header
	cg.output.WriteString(".intel_syntax noprefix\n")
	cg.output.WriteString(".global _start\n\n")

	// Generate data section for string literals
	cg.output.WriteString(".section .data\n")
	cg.generateBuiltinStrings()
	cg.output.WriteString("\n")

	// Generate text section
	cg.output.WriteString(".section .text\n\n")

	// Generate functions
	for _, function := range program.Functions {
		if err := cg.generateFunction(function); err != nil {
			return "", err
		}
	}

	// Generate main entry point that calls main function
	cg.output.WriteString("_start:\n")
	cg.output.WriteString("    call main\n")
	cg.output.WriteString("    mov rax, 60      # sys_exit\n")
	cg.output.WriteString("    mov rdi, 0       # exit status\n")
	cg.output.WriteString("    syscall\n\n")

	// Generate built-in functions
	cg.generateBuiltinFunctions()

	return cg.output.String(), nil
}

// generateBuiltinStrings generates string literals for built-in functions
func (cg *CodeGenerator) generateBuiltinStrings() {
	cg.output.WriteString("newline_str: .ascii \"\\n\"\n")
	cg.output.WriteString("newline_len = . - newline_str\n")
}

// generateFunction generates assembly for a function
func (cg *CodeGenerator) generateFunction(function *ast.FunctionDeclaration) error {
	cg.currentFunc = function.Name
	cg.stackOffset = 0
	cg.variables = make(map[string]int)
	cg.arrays = make(map[string]int)

	// Function label
	cg.output.WriteString(fmt.Sprintf("%s:\n", function.Name))

	// Function prologue
	cg.output.WriteString("    push rbp\n")
	cg.output.WriteString("    mov rbp, rsp\n")

	// Reserve space for parameters (if any)
	for i, param := range function.Parameters {
		cg.stackOffset -= 8 // Each variable takes 8 bytes
		cg.variables[param] = cg.stackOffset
		// Parameters are passed in registers, move them to stack
		switch i {
		case 0:
			cg.output.WriteString(fmt.Sprintf("    mov [rbp%d], rdi\n", cg.stackOffset))
		case 1:
			cg.output.WriteString(fmt.Sprintf("    mov [rbp%d], rsi\n", cg.stackOffset))
			// Add more parameter registers as needed
		}
	}

	// Generate function body
	if err := cg.generateBlockStatement(function.Body); err != nil {
		return err
	}

	// Function epilogue (in case there's no explicit return)
	cg.output.WriteString("    mov rsp, rbp\n")
	cg.output.WriteString("    pop rbp\n")
	cg.output.WriteString("    ret\n\n")

	return nil
}

// generateBlockStatement generates assembly for a block statement
func (cg *CodeGenerator) generateBlockStatement(block *ast.BlockStatement) error {
	for _, stmt := range block.Statements {
		if err := cg.generateStatement(stmt); err != nil {
			return err
		}
	}
	return nil
}

// generateStatement generates assembly for a statement
func (cg *CodeGenerator) generateStatement(stmt ast.Statement) error {
	switch s := stmt.(type) {
	case *ast.VariableDeclaration:
		return cg.generateVariableDeclaration(s)
	case *ast.AssignmentStatement:
		return cg.generateAssignmentStatement(s)
	case *ast.IfStatement:
		return cg.generateIfStatement(s)
	case *ast.WhileStatement:
		return cg.generateWhileStatement(s)
	case *ast.ForStatement:
		return cg.generateForStatement(s)
	case *ast.ReturnStatement:
		return cg.generateReturnStatement(s)
	case *ast.ExpressionStatement:
		return cg.generateExpressionStatement(s)
	default:
		return fmt.Errorf("unsupported statement type: %T", stmt)
	}
}

// generateVariableDeclaration generates assembly for variable declaration
func (cg *CodeGenerator) generateVariableDeclaration(decl *ast.VariableDeclaration) error {
	if decl.IsArray {
		// Handle array declaration
		if arrayLit, ok := decl.Value.(*ast.ArrayLiteral); ok {
			size := len(arrayLit.Elements)
			cg.arrays[decl.Name] = size

			// Allocate space for array (each element is 8 bytes)
			arraySize := size * 8
			cg.stackOffset -= arraySize
			baseOffset := cg.stackOffset
			cg.variables[decl.Name] = baseOffset

			// Initialize array elements
			for i, element := range arrayLit.Elements {
				if err := cg.generateExpression(element); err != nil {
					return err
				}
				elementOffset := baseOffset + (i * 8)
				cg.output.WriteString(fmt.Sprintf("    mov [rbp%d], rax\n", elementOffset))
			}
		}
	} else {
		// Handle regular variable declaration
		cg.stackOffset -= 8
		cg.variables[decl.Name] = cg.stackOffset

		// Generate code for the initial value
		if err := cg.generateExpression(decl.Value); err != nil {
			return err
		}

		// Store the value in the variable's stack location
		cg.output.WriteString(fmt.Sprintf("    mov [rbp%d], rax\n", cg.stackOffset))
	}

	return nil
}

// generateAssignmentStatement generates assembly for assignment
func (cg *CodeGenerator) generateAssignmentStatement(assign *ast.AssignmentStatement) error {
	// Generate code for the value
	if err := cg.generateExpression(assign.Value); err != nil {
		return err
	}

	// Store the result based on the target type
	switch target := assign.Target.(type) {
	case *ast.Identifier:
		// Simple variable assignment
		if offset, exists := cg.variables[target.Value]; exists {
			cg.output.WriteString(fmt.Sprintf("    mov [rbp%d], rax\n", offset))
		} else {
			return fmt.Errorf("undefined variable: %s", target.Value)
		}
	case *ast.IndexExpression:
		// Array element assignment
		if ident, ok := target.Left.(*ast.Identifier); ok {
			if baseOffset, exists := cg.variables[ident.Value]; exists {
				// Push the value to assign
				cg.output.WriteString("    push rax\n")

				// Generate code for the index
				if err := cg.generateExpression(target.Index); err != nil {
					return err
				}

				// Calculate the address: baseOffset + (index * 8)
				cg.output.WriteString("    mov rbx, rax\n")                         // index in rbx
				cg.output.WriteString("    imul rbx, 8\n")                          // multiply by 8 (element size)
				cg.output.WriteString(fmt.Sprintf("    add rbx, %d\n", baseOffset)) // add base offset

				// Pop the value and store it
				cg.output.WriteString("    pop rax\n")
				cg.output.WriteString("    mov [rbp + rbx], rax\n")
			} else {
				return fmt.Errorf("undefined array: %s", ident.Value)
			}
		}
	default:
		return fmt.Errorf("unsupported assignment target: %T", target)
	}

	return nil
}

// generateIfStatement generates assembly for if statement
func (cg *CodeGenerator) generateIfStatement(ifStmt *ast.IfStatement) error {
	elseLabel := cg.newLabel("else")
	endLabel := cg.newLabel("end_if")

	// Generate condition
	if err := cg.generateExpression(ifStmt.Condition); err != nil {
		return err
	}

	// Test the condition and jump to else if false
	cg.output.WriteString("    test rax, rax\n")
	cg.output.WriteString(fmt.Sprintf("    jz %s\n", elseLabel))

	// Generate then body
	if err := cg.generateBlockStatement(ifStmt.ThenBody); err != nil {
		return err
	}

	// Jump to end
	cg.output.WriteString(fmt.Sprintf("    jmp %s\n", endLabel))

	// Else label
	cg.output.WriteString(fmt.Sprintf("%s:\n", elseLabel))

	// Generate else body if it exists
	if ifStmt.ElseBody != nil {
		if err := cg.generateBlockStatement(ifStmt.ElseBody); err != nil {
			return err
		}
	}

	// End label
	cg.output.WriteString(fmt.Sprintf("%s:\n", endLabel))

	return nil
}

// generateWhileStatement generates assembly for while statement
func (cg *CodeGenerator) generateWhileStatement(whileStmt *ast.WhileStatement) error {
	loopLabel := cg.newLabel("loop")
	endLabel := cg.newLabel("end_loop")

	// Loop label
	cg.output.WriteString(fmt.Sprintf("%s:\n", loopLabel))

	// Generate condition
	if err := cg.generateExpression(whileStmt.Condition); err != nil {
		return err
	}

	// Test condition and exit if false
	cg.output.WriteString("    test rax, rax\n")
	cg.output.WriteString(fmt.Sprintf("    jz %s\n", endLabel))

	// Generate loop body
	if err := cg.generateBlockStatement(whileStmt.Body); err != nil {
		return err
	}

	// Jump back to loop condition
	cg.output.WriteString(fmt.Sprintf("    jmp %s\n", loopLabel))

	// End label
	cg.output.WriteString(fmt.Sprintf("%s:\n", endLabel))

	return nil
}

// generateForStatement generates assembly for for statement
func (cg *CodeGenerator) generateForStatement(forStmt *ast.ForStatement) error {
	loopLabel := cg.newLabel("for_loop")
	endLabel := cg.newLabel("end_for")

	// Generate initialization
	if err := cg.generateStatement(forStmt.Init); err != nil {
		return err
	}

	// Loop label
	cg.output.WriteString(fmt.Sprintf("%s:\n", loopLabel))

	// Generate condition
	if err := cg.generateExpression(forStmt.Condition); err != nil {
		return err
	}

	// Test condition and exit if false
	cg.output.WriteString("    test rax, rax\n")
	cg.output.WriteString(fmt.Sprintf("    jz %s\n", endLabel))

	// Generate loop body
	if err := cg.generateBlockStatement(forStmt.Body); err != nil {
		return err
	}

	// Generate update
	if err := cg.generateStatement(forStmt.Update); err != nil {
		return err
	}

	// Jump back to condition
	cg.output.WriteString(fmt.Sprintf("    jmp %s\n", loopLabel))

	// End label
	cg.output.WriteString(fmt.Sprintf("%s:\n", endLabel))

	return nil
}

// generateReturnStatement generates assembly for return statement
func (cg *CodeGenerator) generateReturnStatement(ret *ast.ReturnStatement) error {
	if ret.Value != nil {
		// Generate code for return value
		if err := cg.generateExpression(ret.Value); err != nil {
			return err
		}
		// Return value is already in rax
	} else {
		// Return 0 if no value specified
		cg.output.WriteString("    mov rax, 0\n")
	}

	// Function epilogue
	cg.output.WriteString("    mov rsp, rbp\n")
	cg.output.WriteString("    pop rbp\n")
	cg.output.WriteString("    ret\n")

	return nil
}

// generateExpressionStatement generates assembly for expression statement
func (cg *CodeGenerator) generateExpressionStatement(expr *ast.ExpressionStatement) error {
	return cg.generateExpression(expr.Expression)
}

// generateExpression generates assembly for an expression
func (cg *CodeGenerator) generateExpression(expr ast.Expression) error {
	switch e := expr.(type) {
	case *ast.IntegerLiteral:
		cg.output.WriteString(fmt.Sprintf("    mov rax, %d\n", e.Value))

	case *ast.BooleanLiteral:
		if e.Value {
			cg.output.WriteString("    mov rax, 1\n")
		} else {
			cg.output.WriteString("    mov rax, 0\n")
		}

	case *ast.StringLiteral:
		// For string literals, we'll store the address
		// This is a simplified implementation
		cg.output.WriteString(fmt.Sprintf("    mov rax, %s\n", e.Value))

	case *ast.Identifier:
		if offset, exists := cg.variables[e.Value]; exists {
			cg.output.WriteString(fmt.Sprintf("    mov rax, [rbp%d]\n", offset))
		} else {
			return fmt.Errorf("undefined variable: %s", e.Value)
		}

	case *ast.BinaryExpression:
		return cg.generateBinaryExpression(e)

	case *ast.UnaryExpression:
		return cg.generateUnaryExpression(e)

	case *ast.CallExpression:
		return cg.generateCallExpression(e)

	case *ast.IndexExpression:
		return cg.generateIndexExpression(e)

	case *ast.DotExpression:
		return cg.generateDotExpression(e)

	case *ast.ArrayLiteral:
		// Array literals are handled in variable declarations
		return fmt.Errorf("array literals not supported in expressions")

	default:
		return fmt.Errorf("unsupported expression type: %T", expr)
	}

	return nil
}

// generateBinaryExpression generates assembly for binary expressions
func (cg *CodeGenerator) generateBinaryExpression(expr *ast.BinaryExpression) error {
	// Generate code for left operand
	if err := cg.generateExpression(expr.Left); err != nil {
		return err
	}

	// Push left operand to stack
	cg.output.WriteString("    push rax\n")

	// Generate code for right operand
	if err := cg.generateExpression(expr.Right); err != nil {
		return err
	}

	// Pop left operand into rbx
	cg.output.WriteString("    mov rbx, rax\n") // right operand in rbx
	cg.output.WriteString("    pop rax\n")      // left operand in rax

	// Generate operation
	switch expr.Operator {
	case lexer.PLUS:
		cg.output.WriteString("    add rax, rbx\n")
	case lexer.MINUS:
		cg.output.WriteString("    sub rax, rbx\n")
	case lexer.MULTIPLY:
		cg.output.WriteString("    imul rax, rbx\n")
	case lexer.DIVIDE:
		cg.output.WriteString("    cqo\n")      // Sign extend rax into rdx:rax
		cg.output.WriteString("    idiv rbx\n") // Divide rdx:rax by rbx
	case lexer.EQUAL:
		cg.output.WriteString("    cmp rax, rbx\n")
		cg.output.WriteString("    sete al\n")       // Set al to 1 if equal
		cg.output.WriteString("    movzx rax, al\n") // Zero extend al to rax
	case lexer.NOT_EQUAL:
		cg.output.WriteString("    cmp rax, rbx\n")
		cg.output.WriteString("    setne al\n")
		cg.output.WriteString("    movzx rax, al\n")
	case lexer.LESS_THAN:
		cg.output.WriteString("    cmp rax, rbx\n")
		cg.output.WriteString("    setl al\n")
		cg.output.WriteString("    movzx rax, al\n")
	case lexer.GREATER_THAN:
		cg.output.WriteString("    cmp rax, rbx\n")
		cg.output.WriteString("    setg al\n")
		cg.output.WriteString("    movzx rax, al\n")
	case lexer.LESS_EQUAL:
		cg.output.WriteString("    cmp rax, rbx\n")
		cg.output.WriteString("    setle al\n")
		cg.output.WriteString("    movzx rax, al\n")
	case lexer.GREATER_EQUAL:
		cg.output.WriteString("    cmp rax, rbx\n")
		cg.output.WriteString("    setge al\n")
		cg.output.WriteString("    movzx rax, al\n")
	case lexer.LOGICAL_AND:
		// Short-circuit evaluation: if left is false, result is false
		falseLabel := cg.newLabel("and_false")
		endLabel := cg.newLabel("and_end")
		cg.output.WriteString("    test rax, rax\n")
		cg.output.WriteString(fmt.Sprintf("    jz %s\n", falseLabel))
		cg.output.WriteString("    test rbx, rbx\n")
		cg.output.WriteString(fmt.Sprintf("    jz %s\n", falseLabel))
		cg.output.WriteString("    mov rax, 1\n")
		cg.output.WriteString(fmt.Sprintf("    jmp %s\n", endLabel))
		cg.output.WriteString(fmt.Sprintf("%s:\n", falseLabel))
		cg.output.WriteString("    mov rax, 0\n")
		cg.output.WriteString(fmt.Sprintf("%s:\n", endLabel))
	case lexer.LOGICAL_OR:
		// Short-circuit evaluation: if left is true, result is true
		trueLabel := cg.newLabel("or_true")
		endLabel := cg.newLabel("or_end")
		cg.output.WriteString("    test rax, rax\n")
		cg.output.WriteString(fmt.Sprintf("    jnz %s\n", trueLabel))
		cg.output.WriteString("    test rbx, rbx\n")
		cg.output.WriteString(fmt.Sprintf("    jnz %s\n", trueLabel))
		cg.output.WriteString("    mov rax, 0\n")
		cg.output.WriteString(fmt.Sprintf("    jmp %s\n", endLabel))
		cg.output.WriteString(fmt.Sprintf("%s:\n", trueLabel))
		cg.output.WriteString("    mov rax, 1\n")
		cg.output.WriteString(fmt.Sprintf("%s:\n", endLabel))
	default:
		return fmt.Errorf("unsupported binary operator: %v", expr.Operator)
	}

	return nil
}

// generateUnaryExpression generates assembly for unary expressions
func (cg *CodeGenerator) generateUnaryExpression(expr *ast.UnaryExpression) error {
	// Generate code for the operand
	if err := cg.generateExpression(expr.Right); err != nil {
		return err
	}

	switch expr.Operator {
	case lexer.MINUS:
		cg.output.WriteString("    neg rax\n")
	case lexer.LOGICAL_NOT:
		cg.output.WriteString("    test rax, rax\n")
		cg.output.WriteString("    setz al\n")       // Set al to 1 if rax is zero
		cg.output.WriteString("    movzx rax, al\n") // Zero extend al to rax
	default:
		return fmt.Errorf("unsupported unary operator: %v", expr.Operator)
	}

	return nil
}

// generateCallExpression generates assembly for function calls
func (cg *CodeGenerator) generateCallExpression(expr *ast.CallExpression) error {
	if ident, ok := expr.Function.(*ast.Identifier); ok {
		switch ident.Value {
		case "print":
			// Built-in print function
			if len(expr.Arguments) != 1 {
				return fmt.Errorf("print function expects exactly 1 argument")
			}

			// Generate code for the argument
			if err := cg.generateExpression(expr.Arguments[0]); err != nil {
				return err
			}

			// Call the built-in print function
			cg.output.WriteString("    call print_int\n")

		default:
			// User-defined function call
			// Generate arguments (simplified - assumes up to 2 arguments)
			for i, arg := range expr.Arguments {
				if err := cg.generateExpression(arg); err != nil {
					return err
				}

				// Move argument to appropriate register
				switch i {
				case 0:
					cg.output.WriteString("    mov rdi, rax\n")
				case 1:
					cg.output.WriteString("    mov rsi, rax\n")
					// Add more argument registers as needed
				}
			}

			// Call the function
			cg.output.WriteString(fmt.Sprintf("    call %s\n", ident.Value))
		}
	}

	return nil
}

// generateIndexExpression generates assembly for array indexing
func (cg *CodeGenerator) generateIndexExpression(expr *ast.IndexExpression) error {
	if ident, ok := expr.Left.(*ast.Identifier); ok {
		if baseOffset, exists := cg.variables[ident.Value]; exists {
			// Generate code for the index
			if err := cg.generateExpression(expr.Index); err != nil {
				return err
			}

			// Calculate the address: baseOffset + (index * 8)
			cg.output.WriteString("    mov rbx, rax\n")                         // index in rbx
			cg.output.WriteString("    imul rbx, 8\n")                          // multiply by 8 (element size)
			cg.output.WriteString(fmt.Sprintf("    add rbx, %d\n", baseOffset)) // add base offset

			// Load the value from the calculated address
			cg.output.WriteString("    mov rax, [rbp + rbx]\n")
		} else {
			return fmt.Errorf("undefined array: %s", ident.Value)
		}
	} else {
		return fmt.Errorf("unsupported array indexing")
	}

	return nil
}

// generateDotExpression generates assembly for dot notation (builtin functions)
func (cg *CodeGenerator) generateDotExpression(expr *ast.DotExpression) error {
	if ident, ok := expr.Left.(*ast.Identifier); ok {
		switch expr.Property {
		case "length":
			// Get array length
			if size, exists := cg.arrays[ident.Value]; exists {
				cg.output.WriteString(fmt.Sprintf("    mov rax, %d\n", size))
			} else {
				return fmt.Errorf("undefined array: %s", ident.Value)
			}
		default:
			return fmt.Errorf("unsupported property: %s", expr.Property)
		}
	} else {
		return fmt.Errorf("unsupported dot expression")
	}

	return nil
}

// generateBuiltinFunctions generates the built-in functions
func (cg *CodeGenerator) generateBuiltinFunctions() {
	// print_int function - prints an integer followed by newline
	cg.output.WriteString("print_int:\n")
	cg.output.WriteString("    push rbp\n")
	cg.output.WriteString("    mov rbp, rsp\n")
	cg.output.WriteString("    sub rsp, 32      # Reserve space for number conversion\n")
	cg.output.WriteString("    \n")
	cg.output.WriteString("    # Convert integer to string\n")
	cg.output.WriteString("    mov rbx, 10      # Divisor\n")
	cg.output.WriteString("    lea rsi, [rbp-32] # Buffer for digits\n")
	cg.output.WriteString("    add rsi, 31      # Point to end of buffer\n")
	cg.output.WriteString("    mov byte ptr [rsi], 0  # Null terminator\n")
	cg.output.WriteString("    dec rsi\n")
	cg.output.WriteString("    \n")
	cg.output.WriteString("    # Handle negative numbers\n")
	cg.output.WriteString("    mov rcx, 0       # Sign flag\n")
	cg.output.WriteString("    test rax, rax\n")
	cg.output.WriteString("    jns convert_loop\n")
	cg.output.WriteString("    mov rcx, 1       # Set sign flag\n")
	cg.output.WriteString("    neg rax          # Make positive\n")
	cg.output.WriteString("    \n")
	cg.output.WriteString("convert_loop:\n")
	cg.output.WriteString("    xor rdx, rdx     # Clear rdx for division\n")
	cg.output.WriteString("    div rbx          # Divide by 10\n")
	cg.output.WriteString("    add dl, '0'      # Convert remainder to ASCII\n")
	cg.output.WriteString("    mov [rsi], dl    # Store digit\n")
	cg.output.WriteString("    dec rsi\n")
	cg.output.WriteString("    test rax, rax    # Check if quotient is 0\n")
	cg.output.WriteString("    jnz convert_loop\n")
	cg.output.WriteString("    \n")
	cg.output.WriteString("    # Add minus sign if negative\n")
	cg.output.WriteString("    test rcx, rcx\n")
	cg.output.WriteString("    jz print_number\n")
	cg.output.WriteString("    mov byte ptr [rsi], '-'\n")
	cg.output.WriteString("    dec rsi\n")
	cg.output.WriteString("    \n")
	cg.output.WriteString("print_number:\n")
	cg.output.WriteString("    inc rsi          # Point to first character\n")
	cg.output.WriteString("    \n")
	cg.output.WriteString("    # Calculate string length\n")
	cg.output.WriteString("    lea rdi, [rbp-32]\n")
	cg.output.WriteString("    add rdi, 31      # Point to end\n")
	cg.output.WriteString("    sub rdi, rsi     # Length = end - start\n")
	cg.output.WriteString("    mov rdx, rdi     # Length in rdx\n")
	cg.output.WriteString("    \n")
	cg.output.WriteString("    # System call to write\n")
	cg.output.WriteString("    mov rax, 1       # sys_write\n")
	cg.output.WriteString("    mov rdi, 1       # stdout\n")
	cg.output.WriteString("    # rsi already points to string\n")
	cg.output.WriteString("    # rdx already contains length\n")
	cg.output.WriteString("    syscall\n")
	cg.output.WriteString("    \n")
	cg.output.WriteString("    # Print newline\n")
	cg.output.WriteString("    mov rax, 1       # sys_write\n")
	cg.output.WriteString("    mov rdi, 1       # stdout\n")
	cg.output.WriteString("    mov rsi, newline_str\n")
	cg.output.WriteString("    mov rdx, newline_len\n")
	cg.output.WriteString("    syscall\n")
	cg.output.WriteString("    \n")
	cg.output.WriteString("    mov rsp, rbp\n")
	cg.output.WriteString("    pop rbp\n")
	cg.output.WriteString("    ret\n\n")
}

// newLabel generates a new unique label
func (cg *CodeGenerator) newLabel(prefix string) string {
	cg.labelCounter++
	return fmt.Sprintf("%s_%d", prefix, cg.labelCounter)
}
