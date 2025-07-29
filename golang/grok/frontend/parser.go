package frontend

import (
	"fmt"
	"strconv"
)

// NodeType represents the type of an AST node
type NodeType string

const (
	NodeProgram      NodeType = "Program"
	NodeFunction     NodeType = "Function"
	NodeStmtList     NodeType = "StmtList"
	NodeVarDecl      NodeType = "VarDecl"
	NodeAssign       NodeType = "Assign"
	NodeIfStmt       NodeType = "IfStmt"
	NodeWhileStmt    NodeType = "WhileStmt"
	NodeForStmt      NodeType = "ForStmt"
	NodeReturnStmt   NodeType = "ReturnStmt"
	NodePrintStmt    NodeType = "PrintStmt"
	NodeBinaryExpr   NodeType = "BinaryExpr"
	NodeUnaryExpr    NodeType = "UnaryExpr"
	NodeLiteral      NodeType = "Literal"
	NodeIdentifier   NodeType = "Identifier"
	NodeArrayAccess  NodeType = "ArrayAccess"
	NodeArrayLength  NodeType = "ArrayLength"
	NodeArrayLiteral NodeType = "ArrayLiteral"
	NodeFuncCall     NodeType = "FuncCall"
	NodeExprStmt     NodeType = "ExprStmt"
)

// ASTNode represents a node in the abstract syntax tree
type ASTNode struct {
	Type     NodeType
	Value    interface{}
	Children []*ASTNode
	Token    Token
}

// Parser holds the state for parsing
type Parser struct {
	tokens  []Token
	current int
}

// NewParser creates a new parser
func NewParser(tokens []Token) *Parser {
	return &Parser{tokens: tokens, current: 0}
}

// peek returns the current token
func (p *Parser) peek() Token {
	if p.current >= len(p.tokens) {
		return Token{Line: p.currentLine()}
	}
	return p.tokens[p.current]
}

// consume checks and advances to the next token
func (p *Parser) consume(tType TokenType, value string) error {
	token := p.peek()
	if p.current >= len(p.tokens) || (tType != TokenEOF && token.Type != tType) || (value != "" && token.Literal != value) {
		return fmt.Errorf("expect %s '%s' at line %d, found %s '%s'", tType, value, token.Line, token.Type, token.Literal)
	}
	p.current++
	return nil
}

// currentLine returns the current line number
func (p *Parser) currentLine() int {
	if p.current >= len(p.tokens) {
		return p.tokens[len(p.tokens)-1].Line
	}
	return p.tokens[p.current].Line
}

// Parse constructs the AST
func (p *Parser) Parse() (*ASTNode, error) {
	program := &ASTNode{Type: NodeProgram, Children: []*ASTNode{}}
	for p.current < len(p.tokens) && p.peek().Type != TokenEOF {
		node := p.parseFunction()
		if node != nil {
			program.Children = append(program.Children, node)
		} else {
			return nil, fmt.Errorf("unexpected token at line %d: %s", p.currentLine(), p.peek().Literal)
		}
	}
	return program, nil
}

// parseFunction parses a function declaration
func (p *Parser) parseFunction() *ASTNode {
	if p.peek().Type != TokenFunc {
		return nil
	}
	p.consume(TokenFunc, "func")
	nameToken := p.peek()
	if err := p.consume(TokenIdentifier, ""); err != nil {
		panic(err)
	}
	node := &ASTNode{Type: NodeFunction, Value: nameToken.Literal, Token: nameToken}
	if err := p.consume(TokenLParen, "("); err != nil {
		panic(err)
	}
	// Parse parameters
	for p.peek().Type == TokenInt || p.peek().Type == TokenBool {
		typeToken := p.peek()
		if err := p.consume(typeToken.Type, ""); err != nil {
			panic(err)
		}
		paramName := p.peek()
		if err := p.consume(TokenIdentifier, ""); err != nil {
			panic(err)
		}
		node.Children = append(node.Children, &ASTNode{Type: NodeIdentifier, Value: paramName.Literal, Token: paramName})
		if p.peek().Literal == "," {
			p.consume(TokenComma, ",")
		}
	}
	if err := p.consume(TokenRParen, ")"); err != nil {
		panic(err)
	}
	if err := p.consume(TokenLBrace, "{"); err != nil {
		panic(err)
	}
	body := p.parseStmtList()
	node.Children = append(node.Children, body)
	if err := p.consume(TokenRBrace, "}"); err != nil {
		panic(err)
	}
	return node
}

// parseStmtList parses a list of statements
func (p *Parser) parseStmtList() *ASTNode {
	node := &ASTNode{Type: NodeStmtList}
	for p.peek().Type != TokenRBrace && p.peek().Type != TokenEOF {
		stmt := p.parseStmt()
		if stmt != nil {
			node.Children = append(node.Children, stmt)
		} else {
			break
		}
	}
	return node
}

// parseStmt parses a single statement
func (p *Parser) parseStmt() *ASTNode {
	if p.peek().Type == TokenInt || p.peek().Type == TokenBool {
		return p.parseVarDecl()
	} else if p.peek().Type == TokenIf {
		return p.parseIfStmt()
	} else if p.peek().Type == TokenWhile {
		return p.parseWhileStmt()
	} else if p.peek().Type == TokenFor {
		return p.parseForStmt()
	} else if p.peek().Type == TokenReturn {
		return p.parseReturnStmt()
	} else if p.peek().Type == TokenPrint {
		return p.parsePrintStmt()
	} else if p.peek().Type == TokenIdentifier {
		ident := p.peek()
		p.consume(TokenIdentifier, "")
		if p.peek().Literal == "(" {
			return p.parseFuncCallStmt(ident)
		} else if p.peek().Literal == "[" || p.peek().Literal == "=" {
			return p.parseAssignStmt(ident)
		} else if p.peek().Type == TokenInc || p.peek().Type == TokenDec {
			return p.parseIncDecStmt(ident)
		} else {
			return p.parseFuncCallStmt(ident) // Handle standalone function calls
		}
	}
	return nil
}

// parseVarDecl parses a variable declaration
func (p *Parser) parseVarDecl() *ASTNode {
	keyword := p.peek()
	if err := p.consume(keyword.Type, ""); err != nil {
		panic(err)
	}
	nameToken := p.peek()
	if err := p.consume(TokenIdentifier, ""); err != nil {
		panic(err)
	}
	node := &ASTNode{Type: NodeVarDecl, Value: nameToken.Literal, Token: nameToken}
	if p.peek().Literal == "[" {
		p.consume(TokenLBracket, "[")
		p.consume(TokenRBracket, "]")
	}
	if p.peek().Literal == "=" {
		p.consume(TokenEqual, "=")
		if p.peek().Literal == "[" {
			arrayLit := p.parseArrayLiteral()
			node.Children = append(node.Children, &ASTNode{Type: NodeIdentifier, Value: nameToken.Literal, Token: nameToken}, arrayLit)
		} else {
			expr := p.parseExpr()
			node.Children = append(node.Children, &ASTNode{Type: NodeIdentifier, Value: nameToken.Literal, Token: nameToken}, expr)
		}
	}
	if err := p.consume(TokenSemicolon, ";"); err != nil {
		panic(err)
	}
	return node
}

// parseAssignStmt parses an assignment statement
func (p *Parser) parseAssignStmt(ident Token) *ASTNode {
	node := &ASTNode{Type: NodeAssign, Token: ident}
	target := &ASTNode{Type: NodeIdentifier, Value: ident.Literal, Token: ident}
	if p.peek().Literal == "[" {
		p.consume(TokenLBracket, "[")
		index := p.parseExpr()
		if err := p.consume(TokenRBracket, "]"); err != nil {
			panic(err)
		}
		target = &ASTNode{Type: NodeArrayAccess, Children: []*ASTNode{{Type: NodeIdentifier, Value: ident.Literal, Token: ident}, index}, Token: ident}
	}
	if err := p.consume(TokenEqual, "="); err != nil {
		panic(err)
	}
	expr := p.parseExpr()
	node.Children = append(node.Children, target, expr)
	fmt.Printf("Before semicolon in assign, token: %v\n", p.peek())
	if err := p.consume(TokenSemicolon, ";"); err != nil {
		panic(err)
	}
	fmt.Printf("After semicolon in assign, token: %v\n", p.peek())
	return node
}

// parseIncDecStmt parses an increment or decrement statement
func (p *Parser) parseIncDecStmt(ident Token) *ASTNode {
	op := p.peek()
	if err := p.consume(op.Type, op.Literal); err != nil {
		panic(err)
	}
	if err := p.consume(TokenSemicolon, ";"); err != nil {
		panic(err)
	}
	// Convert j++ or j-- to j = j + 1 or j = j - 1
	opValue := ""
	if op.Type == TokenInc {
		opValue = "+"
	} else {
		opValue = "-"
	}
	one := &ASTNode{Type: NodeLiteral, Value: 1, Token: Token{Type: TokenNumber, Literal: "1"}}
	rhs := &ASTNode{Type: NodeBinaryExpr, Value: opValue, Children: []*ASTNode{
		{Type: NodeIdentifier, Value: ident.Literal, Token: ident}, one}, Token: op}
	return &ASTNode{Type: NodeAssign, Children: []*ASTNode{
		{Type: NodeIdentifier, Value: ident.Literal, Token: ident}, rhs}, Token: ident}
}

// parseIfStmt parses an if statement
func (p *Parser) parseIfStmt() *ASTNode {
	if err := p.consume(TokenIf, "if"); err != nil {
		panic(err)
	}
	if err := p.consume(TokenLParen, "("); err != nil {
		panic(err)
	}
	cond := p.parseExpr()
	if err := p.consume(TokenRParen, ")"); err != nil {
		panic(err)
	}
	if err := p.consume(TokenLBrace, "{"); err != nil {
		panic(err)
	}
	body := p.parseStmtList()
	if err := p.consume(TokenRBrace, "}"); err != nil {
		panic(err)
	}
	node := &ASTNode{Type: NodeIfStmt, Children: []*ASTNode{cond, body}}
	if p.peek().Type == TokenElse {
		p.consume(TokenElse, "else")
		if p.peek().Type == TokenIf {
			elseBody := p.parseIfStmt()
			node.Children = append(node.Children, elseBody)
		} else {
			if err := p.consume(TokenLBrace, "{"); err != nil {
				panic(err)
			}
			elseBody := p.parseStmtList()
			node.Children = append(node.Children, elseBody)
			if err := p.consume(TokenRBrace, "}"); err != nil {
				panic(err)
			}
		}
	}
	return node
}

// parseWhileStmt parses a while statement
func (p *Parser) parseWhileStmt() *ASTNode {
	if err := p.consume(TokenWhile, "while"); err != nil {
		panic(err)
	}
	if err := p.consume(TokenLParen, "("); err != nil {
		panic(err)
	}
	cond := p.parseExpr()
	if err := p.consume(TokenRParen, ")"); err != nil {
		panic(err)
	}
	if err := p.consume(TokenLBrace, "{"); err != nil {
		panic(err)
	}
	body := p.parseStmtList()
	if err := p.consume(TokenRBrace, "}"); err != nil {
		panic(err)
	}
	return &ASTNode{Type: NodeWhileStmt, Children: []*ASTNode{cond, body}}
}

// parseForStmt parses a for statement
func (p *Parser) parseForStmt() *ASTNode {
	if err := p.consume(TokenFor, "for"); err != nil {
		panic(err)
	}
	if err := p.consume(TokenLParen, "("); err != nil {
		panic(err)
	}
	var init, cond, incr *ASTNode
	// Parse initialization clause
	if p.peek().Type != TokenSemicolon {
		init = p.parseStmt()
	} else {
		if err := p.consume(TokenSemicolon, ";"); err != nil {
			panic(err)
		}
	}
	fmt.Printf("After init, token: %v\n", p.peek())
	// Parse condition clause
	if p.peek().Type != TokenSemicolon {
		cond = p.parseExpr()
		if err := p.consume(TokenSemicolon, ";"); err != nil {
			panic(err)
		}
	} else {
		if err := p.consume(TokenSemicolon, ";"); err != nil {
			panic(err)
		}
	}
	fmt.Printf("After cond, token: %v\n", p.peek())
	// Parse update clause
	if p.peek().Type != TokenRParen {
		fmt.Printf("Before incr, token: %v\n", p.peek())
		incr = p.parseStmt()
	}
	// Consume closing parenthesis
	if err := p.consume(TokenRParen, ")"); err != nil {
		panic(err)
	}
	if err := p.consume(TokenLBrace, "{"); err != nil {
		panic(err)
	}
	body := p.parseStmtList()
	if err := p.consume(TokenRBrace, "}"); err != nil {
		panic(err)
	}
	return &ASTNode{Type: NodeForStmt, Children: []*ASTNode{init, cond, incr, body}}
}

// parseReturnStmt parses a return statement
func (p *Parser) parseReturnStmt() *ASTNode {
	if err := p.consume(TokenReturn, "return"); err != nil {
		panic(err)
	}
	node := &ASTNode{Type: NodeReturnStmt}
	if p.peek().Type != TokenSemicolon {
		expr := p.parseExpr()
		node.Children = append(node.Children, expr)
	}
	if err := p.consume(TokenSemicolon, ";"); err != nil {
		panic(err)
	}
	return node
}

// parsePrintStmt parses a print statement
func (p *Parser) parsePrintStmt() *ASTNode {
	if err := p.consume(TokenPrint, "print"); err != nil {
		panic(err)
	}
	if err := p.consume(TokenLParen, "("); err != nil {
		panic(err)
	}
	expr := p.parseExpr()
	if err := p.consume(TokenRParen, ")"); err != nil {
		panic(err)
	}
	if err := p.consume(TokenSemicolon, ";"); err != nil {
		panic(err)
	}
	return &ASTNode{Type: NodePrintStmt, Children: []*ASTNode{expr}}
}

// parseFuncCallStmt parses a function call statement
func (p *Parser) parseFuncCallStmt(ident Token) *ASTNode {
	node := &ASTNode{Type: NodeFuncCall, Value: ident.Literal, Token: ident}
	if p.peek().Literal == "(" {
		if err := p.consume(TokenLParen, "("); err != nil {
			panic(err)
		}
		for p.peek().Type != TokenRParen {
			expr := p.parseExpr()
			node.Children = append(node.Children, expr)
			if p.peek().Literal == "," {
				p.consume(TokenComma, ",")
			}
		}
		if err := p.consume(TokenRParen, ")"); err != nil {
			panic(err)
		}
	}
	if err := p.consume(TokenSemicolon, ";"); err != nil {
		panic(err)
	}
	return node
}

// parseExpr parses an expression
func (p *Parser) parseExpr() *ASTNode {
	return p.parseBinaryExpr(0)
}

// parseBinaryExpr parses a binary expression
func (p *Parser) parseBinaryExpr(precedence int) *ASTNode {
	node := p.parsePrimary()
	for {
		op := p.peek()
		opPrecedence := p.getPrecedence(op.Literal)
		if (op.Type != TokenPlus && op.Type != TokenMinus && op.Type != TokenMultiply && op.Type != TokenDivide && op.Type != TokenDoubleEqual && op.Type != TokenLessThan) || opPrecedence <= precedence {
			break
		}
		p.consume(op.Type, op.Literal)
		rhs := p.parseBinaryExpr(opPrecedence)
		node = &ASTNode{Type: NodeBinaryExpr, Value: op.Literal, Children: []*ASTNode{node, rhs}, Token: op}
	}
	return node
}

// parsePrimary parses a primary expression
func (p *Parser) parsePrimary() *ASTNode {
	if p.peek().Type == TokenNumber {
		token := p.peek()
		p.consume(TokenNumber, "")
		value, _ := strconv.Atoi(token.Literal)
		return &ASTNode{Type: NodeLiteral, Value: value, Token: token}
	} else if p.peek().Type == TokenString {
		token := p.peek()
		p.consume(TokenString, "")
		return &ASTNode{Type: NodeLiteral, Value: token.Literal, Token: token}
	} else if p.peek().Type == TokenTrue || p.peek().Type == TokenFalse {
		token := p.peek()
		p.consume(token.Type, "")
		value := token.Type == TokenTrue
		return &ASTNode{Type: NodeLiteral, Value: value, Token: token}
	} else if p.peek().Type == TokenIdentifier {
		ident := p.peek()
		p.consume(TokenIdentifier, "")
		if p.peek().Literal == "(" {
			node := &ASTNode{Type: NodeFuncCall, Value: ident.Literal, Token: ident}
			p.consume(TokenLParen, "(")
			for p.peek().Type != TokenRParen {
				expr := p.parseExpr()
				node.Children = append(node.Children, expr)
				if p.peek().Literal == "," {
					p.consume(TokenComma, ",")
				}
			}
			p.consume(TokenRParen, ")")
			return node
		} else if p.peek().Literal == "[" {
			p.consume(TokenLBracket, "[")
			index := p.parseExpr()
			p.consume(TokenRBracket, "]")
			return &ASTNode{Type: NodeArrayAccess, Children: []*ASTNode{{Type: NodeIdentifier, Value: ident.Literal, Token: ident}, index}, Token: ident}
		} else if p.peek().Literal == "." && p.peekNext().Literal == "length" {
			p.consume(TokenDot, ".")
			p.consume(TokenLength, "length")
			return &ASTNode{Type: NodeArrayLength, Children: []*ASTNode{{Type: NodeIdentifier, Value: ident.Literal, Token: ident}}, Token: ident}
		}
		return &ASTNode{Type: NodeIdentifier, Value: ident.Literal, Token: ident}
	} else if p.peek().Type == TokenLParen {
		p.consume(TokenLParen, "(")
		expr := p.parseExpr()
		p.consume(TokenRParen, ")")
		return expr
	} else if p.peek().Type == TokenBang {
		token := p.peek()
		p.consume(TokenBang, "!")
		expr := p.parsePrimary()
		return &ASTNode{Type: NodeUnaryExpr, Value: "!", Children: []*ASTNode{expr}, Token: token}
	}
	panic(fmt.Errorf("unexpected token at line %d: %s", p.currentLine(), p.peek().Literal))
}

// parseArrayLiteral parses an array literal
func (p *Parser) parseArrayLiteral() *ASTNode {
	node := &ASTNode{Type: NodeArrayLiteral}
	if err := p.consume(TokenLBracket, "["); err != nil {
		panic(err)
	}
	for p.peek().Type != TokenRBracket {
		expr := p.parseExpr()
		node.Children = append(node.Children, expr)
		if p.peek().Literal == "," {
			p.consume(TokenComma, ",")
		}
	}
	if err := p.consume(TokenRBracket, "]"); err != nil {
		panic(err)
	}
	return node
}

// peekNext returns the next token
func (p *Parser) peekNext() Token {
	if p.current+1 >= len(p.tokens) {
		return Token{Line: p.currentLine()}
	}
	return p.tokens[p.current+1]
}

// getPrecedence returns the precedence of an operator
func (p *Parser) getPrecedence(op string) int {
	switch op {
	case "==", "<":
		return 1
	case "+", "-":
		return 2
	case "*", "/":
		return 3
	default:
		return 0
	}
}
