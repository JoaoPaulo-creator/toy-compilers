package backend

import (
	"fmt"
	"strings"
	"toy-compiler/frontend"
)

// CodeGen holds the state for code generation
type CodeGen struct {
	ast        *frontend.ASTNode
	symbols    map[string]string // Maps variable names to stack offsets
	spOffset   int               // Stack pointer offset
	labelCount int               // For generating unique labels
	output     strings.Builder   // Assembly output
}

// NewCodeGen creates a new code generator
func NewCodeGen(ast *frontend.ASTNode) *CodeGen {
	return &CodeGen{
		ast:     ast,
		symbols: make(map[string]string),
	}
}

// Generate produces x86_64 assembly code
func (cg *CodeGen) Generate() (string, error) {
	// Initialize assembly with necessary directives
	cg.output.WriteString("section .text\n")
	cg.output.WriteString("extern printf\n")
	cg.output.WriteString("global main\n")
	cg.output.WriteString("section .data\n")
	cg.output.WriteString("fmt_int: db \"%d\", 10, 0\n")
	cg.output.WriteString("fmt_str: db \"%s\", 10, 0\n")

	// Generate code for each function
	for _, fn := range cg.ast.Children {
		if err := cg.genFunction(fn); err != nil {
			return "", err
		}
	}

	return cg.output.String(), nil
}

// genFunction generates code for a function
func (cg *CodeGen) genFunction(fn *frontend.ASTNode) error {
	if fn.Type != frontend.NodeFunction {
		return fmt.Errorf("expected function node")
	}

	name := fn.Value.(string)
	cg.output.WriteString(fmt.Sprintf("%s:\n", name))
	cg.output.WriteString("push rbp\n")
	cg.output.WriteString("mov rbp, rsp\n")

	// Allocate space for parameters
	paramCount := len(fn.Children) - 1 // Last child is body
	cg.spOffset = 0
	for i := 0; i < paramCount; i++ {
		param := fn.Children[i]
		varName := param.Value.(string)
		cg.spOffset -= 8
		cg.symbols[varName] = fmt.Sprintf("[rbp%d]", cg.spOffset)
		// Move parameters from registers/stack
		if i < 6 {
			regs := []string{"rdi", "rsi", "rdx", "rcx", "r8", "r9"}
			cg.output.WriteString(fmt.Sprintf("mov %s, %s\n", cg.symbols[varName], regs[i]))
		} else {
			cg.output.WriteString(fmt.Sprintf("mov rax, [rbp+%d]\n", 16+(i-6)*8))
			cg.output.WriteString(fmt.Sprintf("mov %s, rax\n", cg.symbols[varName]))
		}
	}

	// Generate body
	body := fn.Children[paramCount]
	if err := cg.genStmtList(body); err != nil {
		return err
	}

	// Epilogue
	cg.output.WriteString("mov rsp, rbp\n")
	cg.output.WriteString("pop rbp\n")
	if name == "main" {
		cg.output.WriteString("mov rax, 0\n")
	}
	cg.output.WriteString("ret\n")
	return nil
}

// genStmtList generates code for a statement list
func (cg *CodeGen) genStmtList(stmtList *frontend.ASTNode) error {
	for _, stmt := range stmtList.Children {
		if err := cg.genStmt(stmt); err != nil {
			return err
		}
	}
	return nil
}

// genStmt generates code for a single statement
func (cg *CodeGen) genStmt(stmt *frontend.ASTNode) error {
	switch stmt.Type {
	case frontend.NodeVarDecl:
		return cg.genVarDecl(stmt)
	case frontend.NodeAssign:
		return cg.genAssign(stmt)
	case frontend.NodeIfStmt:
		return cg.genIfStmt(stmt)
	case frontend.NodeWhileStmt:
		return cg.genWhileStmt(stmt)
	case frontend.NodeForStmt:
		return cg.genForStmt(stmt)
	case frontend.NodeReturnStmt:
		return cg.genReturnStmt(stmt)
	case frontend.NodePrintStmt:
		return cg.genPrintStmt(stmt)
	default:
		return fmt.Errorf("unknown statement type: %s", stmt.Type)
	}
}

// genVarDecl generates code for a variable declaration
func (cg *CodeGen) genVarDecl(decl *frontend.ASTNode) error {
	varName := decl.Value.(string)
	cg.spOffset -= 8
	cg.symbols[varName] = fmt.Sprintf("[rbp%d]", cg.spOffset)

	if len(decl.Children) > 1 {
		// Has initializer
		expr := decl.Children[1]
		if err := cg.genExpr(expr); err != nil {
			return err
		}
		cg.output.WriteString(fmt.Sprintf("mov %s, rax\n", cg.symbols[varName]))
	}

	if len(decl.Children) > 2 && decl.Children[1].Token.Type == frontend.TokenInt {
		// Array declaration
		arrayLit := decl.Children[2]
		for i, elem := range arrayLit.Children {
			if err := cg.genExpr(elem); err != nil {
				return err
			}
			cg.output.WriteString(fmt.Sprintf("mov [rbp%d+%d], rax\n", cg.spOffset, i*8))
		}
	}

	return nil
}

// genAssign generates code for an assignment
func (cg *CodeGen) genAssign(assign *frontend.ASTNode) error {
	target := assign.Children[0]
	expr := assign.Children[1]

	if target.Type == frontend.NodeArrayAccess {
		// Array assignment
		arr := target.Children[0]
		index := target.Children[1]
		if err := cg.genExpr(index); err != nil {
			return err
		}
		cg.output.WriteString("push rax\n")
		if err := cg.genExpr(expr); err != nil {
			return err
		}
		cg.output.WriteString("mov rbx, rax\n")
		cg.output.WriteString("pop rax\n")
		arrOffset := cg.symbols[arr.Value.(string)]
		cg.output.WriteString(fmt.Sprintf("mov [%s+rax*8], rbx\n", arrOffset))
	} else {
		// Variable assignment
		if err := cg.genExpr(expr); err != nil {
			return err
		}
		cg.output.WriteString(fmt.Sprintf("mov %s, rax\n", cg.symbols[target.Value.(string)]))
	}
	return nil
}

// genIfStmt generates code for an if statement
func (cg *CodeGen) genIfStmt(ifStmt *frontend.ASTNode) error {
	cond := ifStmt.Children[0]
	body := ifStmt.Children[1]
	elseLabel := cg.newLabel()
	endLabel := cg.newLabel()

	if err := cg.genExpr(cond); err != nil {
		return err
	}
	cg.output.WriteString("cmp rax, 0\n")
	cg.output.WriteString(fmt.Sprintf("je %s\n", elseLabel))

	if err := cg.genStmtList(body); err != nil {
		return err
	}
	cg.output.WriteString(fmt.Sprintf("jmp %s\n", endLabel))

	cg.output.WriteString(fmt.Sprintf("%s:\n", elseLabel))
	if len(ifStmt.Children) > 2 {
		elseBody := ifStmt.Children[2]
		if err := cg.genStmtList(elseBody); err != nil {
			return err
		}
	}
	cg.output.WriteString(fmt.Sprintf("%s:\n", endLabel))

	return nil
}

// genWhileStmt generates code for a while statement
func (cg *CodeGen) genWhileStmt(whileStmt *frontend.ASTNode) error {
	cond := whileStmt.Children[0]
	body := whileStmt.Children[1]
	startLabel := cg.newLabel()
	endLabel := cg.newLabel()

	cg.output.WriteString(fmt.Sprintf("%s:\n", startLabel))
	if err := cg.genExpr(cond); err != nil {
		return err
	}
	cg.output.WriteString("cmp rax, 0\n")
	cg.output.WriteString(fmt.Sprintf("je %s\n", endLabel))

	if err := cg.genStmtList(body); err != nil {
		return err
	}
	cg.output.WriteString(fmt.Sprintf("jmp %s\n", startLabel))
	cg.output.WriteString(fmt.Sprintf("%s:\n", endLabel))

	return nil
}

// genForStmt generates code for a for statement
func (cg *CodeGen) genForStmt(forStmt *frontend.ASTNode) error {
	startLabel := cg.newLabel()
	endLabel := cg.newLabel()

	if forStmt.Children[0] != nil {
		if err := cg.genStmt(forStmt.Children[0]); err != nil {
			return err
		}
	}

	cg.output.WriteString(fmt.Sprintf("%s:\n", startLabel))
	if forStmt.Children[1] != nil {
		if err := cg.genExpr(forStmt.Children[1]); err != nil {
			return err
		}
		cg.output.WriteString("cmp rax, 0\n")
		cg.output.WriteString(fmt.Sprintf("je %s\n", endLabel))
	}

	if err := cg.genStmtList(forStmt.Children[3]); err != nil {
		return err
	}

	if forStmt.Children[2] != nil {
		if err := cg.genStmt(forStmt.Children[2]); err != nil {
			return err
		}
	}
	cg.output.WriteString(fmt.Sprintf("jmp %s\n", startLabel))
	cg.output.WriteString(fmt.Sprintf("%s:\n", endLabel))

	return nil
}

// genReturnStmt generates code for a return statement
func (cg *CodeGen) genReturnStmt(returnStmt *frontend.ASTNode) error {
	if len(returnStmt.Children) > 0 && returnStmt.Children[0] != nil {
		if err := cg.genExpr(returnStmt.Children[0]); err != nil {
			return err
		}
	}
	cg.output.WriteString("mov rsp, rbp\n")
	cg.output.WriteString("pop rbp\n")
	cg.output.WriteString("ret\n")
	return nil
}

// genPrintStmt generates code for a print statement
func (cg *CodeGen) genPrintStmt(printStmt *frontend.ASTNode) error {
	expr := printStmt.Children[0]
	if err := cg.genExpr(expr); err != nil {
		return err
	}

	// Determine format based on expression type
	if expr.Type == frontend.NodeLiteral && expr.Value.(type) == string {
		cg.output.WriteString("lea rdi, [rel fmt_str]\n")
	} else {
		cg.output.WriteString("lea rdi, [rel fmt_int]\n")
	}
	cg.output.WriteString("mov rsi, rax\n")
	cg.output.WriteString("xor rax, rax\n")
	cg.output.WriteString("call printf\n")
	return nil
}

// genExpr generates code for an expression
func (cg *CodeGen) genExpr(expr *frontend.ASTNode) error {
	switch expr.Type {
	case frontend.NodeLiteral:
		switch v := expr.Value.(type) {
		case int:
			cg.output.WriteString(fmt.Sprintf("mov rax, %d\n", v))
		case bool:
			if v {
				cg.output.WriteString("mov rax, 1\n")
			} else {
				cg.output.WriteString("mov rax, 0\n")
			}
		case string:
			// Store string in data section
			strLabel := cg.newLabel()
			cg.output.WriteString(fmt.Sprintf("section .data\n%s: db \"%s\", 0\nsection .text\n", strLabel, v))
			cg.output.WriteString(fmt.Sprintf("lea rax, [rel %s]\n", strLabel))
		}
	case frontend.NodeIdentifier:
		cg.output.WriteString(fmt.Sprintf("mov rax, %s\n", cg.symbols[expr.Value.(string)]))
	case frontend.NodeBinaryExpr:
		return cg.genBinaryExpr(expr)
	case frontend.NodeUnaryExpr:
		return cg.genUnaryExpr(expr)
	case frontend.NodeArrayAccess:
		arr := expr.Children[0]
		index := expr.Children[1]
		if err := cg.genExpr(index); err != nil {
			return err
		}
		cg.output.WriteString("mov rbx, rax\n")
		arrOffset := cg.symbols[arr.Value.(string)]
		cg.output.WriteString(fmt.Sprintf("mov rax, [%s+rbx*8]\n", arrOffset))
	case frontend.NodeArrayLength:
		arr := expr.Children[0]
		// For simplicity, assume length is stored at array base -8
		arrOffset := cg.symbols[arr.Value.(string)]
		cg.output.WriteString(fmt.Sprintf("mov rax, [%s-8]\n", arrOffset))
	default:
		return fmt.Errorf("unknown expression type: %s", expr.Type)
	}
	return nil
}

// genBinaryExpr generates code for a binary expression
func (cg *CodeGen) genBinaryExpr(expr *frontend.ASTNode) error {
	if err := cg.genExpr(expr.Children[0]); err != nil {
		return err
	}
	cg.output.WriteString("push rax\n")
	if err := cg.genExpr(expr.Children[1]); err != nil {
		return err
	}
	cg.output.WriteString("mov rbx, rax\n")
	cg.output.WriteString("pop rax\n")

	switch expr.Value.(string) {
	case "+":
		cg.output.WriteString("add rax, rbx\n")
	case "-":
		cg.output.WriteString("sub rax, rbx\n")
	case "*":
		cg.output.WriteString("imul rax, rbx\n")
	case "/":
		cg.output.WriteString("xor rdx, rdx\n")
		cg.output.WriteString("div rbx\n")
	case "==":
		cg.output.WriteString("cmp rax, rbx\n")
		cg.output.WriteString("sete al\n")
		cg.output.WriteString("movzx rax, al\n")
	case "<":
		cg.output.WriteString("cmp rax, rbx\n")
		cg.output.WriteString("setl al\n")
		cg.output.WriteString("movzx rax, al\n")
	default:
		return fmt.Errorf("unknown binary operator: %s", expr.Value)
	}
	return nil
}

// genUnaryExpr generates code for a unary expression
func (cg *CodeGen) genUnaryExpr(expr *frontend.ASTNode) error {
	if err := cg.genExpr(expr.Children[0]); err != nil {
		return err
	}
	if expr.Value.(string) == "!" {
		cg.output.WriteString("cmp rax, 0\n")
		cg.output.WriteString("sete al\n")
		cg.output.WriteString("movzx rax, al\n")
	}
	return nil
}

// newLabel generates a unique label
func (cg *CodeGen) newLabel() string {
	cg.labelCount++
	return fmt.Sprintf(".L%d", cg.labelCount)
}
