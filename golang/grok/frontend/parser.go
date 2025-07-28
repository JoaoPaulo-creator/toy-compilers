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
	return &Parser{tokens: tokens}
}

// Parse performs syntactic analysis and returns the AST
func (p *Parser) Parse() (*ASTNode, error) {
	program := &ASTNode{Type: NodeProgram}
	for !p.isAtEnd() {
		if p.peek().Type == TokenFunc {
			fn, err := p.parseFunction()
			if err != nil {
				return nil, err
			}
			program.Children = append(program.Children, fn)
		} else {
			return nil, fmt.Errorf("unexpected token at line %d: %s", p.peek().Line, p.peek().Literal)
		}
	}
	return program, nil
}

// parseFunction parses a function declaration
func (p *Parser) parseFunction() (*ASTNode, error) {
	p.consume(TokenFunc, "expect 'func'")
	name := p.consume(TokenIdentifier, "expect function name")

	// Parse parameters
	p.consume(TokenLParen, "expect '('")
	var params []*ASTNode
	if p.peek().Type != TokenRParen {
		for {
			var paramType *ASTNode
			// Check if the next token is a type; if not, assume 'int'
			if p.peek().Type == TokenInt || p.peek().Type == TokenBool {
				paramType = p.parseType()
			} else {
				// Default to int type
				paramType = &ASTNode{
					Type:  NodeLiteral,
					Value: "int",
					Token: Token{Type: TokenInt, Literal: "int"},
				}
			}
			paramName := p.consume(TokenIdentifier, "expect parameter name")
			params = append(params, &ASTNode{
				Type:     NodeVarDecl,
				Value:    paramName.Literal,
				Children: []*ASTNode{paramType},
				Token:    paramName,
			})
			if p.peek().Type != TokenComma {
				break
			}
			p.consume(TokenComma, "expect ','")
		}
	}
	p.consume(TokenRParen, "expect ')'")

	// Parse body
	p.consume(TokenLBrace, "expect '{'")
	body, err := p.parseStmtList()
	if err != nil {
		return nil, err
	}
	p.consume(TokenRBrace, "expect '}'")

	return &ASTNode{
		Type:     NodeFunction,
		Value:    name.Literal,
		Children: append(params, body),
		Token:    name,
	}, nil
}

// parseStmtList parses a list of statements
func (p *Parser) parseStmtList() (*ASTNode, error) {
	stmtList := &ASTNode{Type: NodeStmtList}
	for p.peek().Type != TokenRBrace && !p.isAtEnd() {
		stmt, err := p.parseStmt()
		if err != nil {
			return nil, err
		}
		stmtList.Children = append(stmtList.Children, stmt)
	}
	return stmtList, nil
}

// parseStmt parses a single statement
func (p *Parser) parseStmt() (*ASTNode, error) {
	switch p.peek().Type {
	case TokenInt, TokenBool:
		return p.parseVarDecl()
	case TokenIf:
		return p.parseIfStmt()
	case TokenWhile:
		return p.parseWhileStmt()
	case TokenFor:
		return p.parseForStmt()
	case TokenReturn:
		return p.parseReturnStmt()
	case TokenPrint:
		return p.parsePrintStmt()
	case TokenIdentifier:
		if p.peekN(1).Type == TokenLBracket || p.peekN(1).Type == TokenEqual {
			return p.parseAssign()
		}
		return nil, fmt.Errorf("unexpected identifier at line %d: %s", p.peek().Line, p.peek().Literal)
	default:
		return nil, fmt.Errorf("unexpected token at line %d: %s", p.peek().Line, p.peek().Literal)
	}
}

// parseVarDecl parses a variable declaration
func (p *Parser) parseVarDecl() (*ASTNode, error) {
	varType := p.parseType()
	name := p.consume(TokenIdentifier, "expect variable name")
	var decl = &ASTNode{
		Type:     NodeVarDecl,
		Value:    name.Literal,
		Children: []*ASTNode{varType},
		Token:    name,
	}

	if p.peek().Type == TokenEqual {
		p.consume(TokenEqual, "expect '='")
		expr, err := p.parseExpr()
		if err != nil {
			return nil, err
		}
		decl.Children = append(decl.Children, expr)
	}

	if p.peek().Type == TokenLBracket && varType.Token.Type == TokenInt {
		// Array declaration
		p.consume(TokenLBracket, "expect '['")
		p.consume(TokenRBracket, "expect ']'")
		if p.peek().Type == TokenEqual {
			p.consume(TokenEqual, "expect '='")
			arrayLit, err := p.parseArrayLiteral()
			if err != nil {
				return nil, err
			}
			decl.Children = append(decl.Children, arrayLit)
		}
	}

	p.consume(TokenSemicolon, "expect ';'")
	return decl, nil
}

// parseIfStmt parses an if statement
func (p *Parser) parseIfStmt() (*ASTNode, error) {
	p.consume(TokenIf, "expect 'if'")
	p.consume(TokenLParen, "expect '('")
	cond, err := p.parseExpr()
	if err != nil {
		return nil, err
	}
	p.consume(TokenRParen, "expect ')'")
	p.consume(TokenLBrace, "expect '{'")
	body, err := p.parseStmtList()
	if err != nil {
		return nil, err
	}
	p.consume(TokenRBrace, "expect '}'")

	ifNode := &ASTNode{
		Type:     NodeIfStmt,
		Children: []*ASTNode{cond, body},
	}

	if p.peek().Type == TokenElse {
		p.consume(TokenElse, "expect 'else'")
		if p.peek().Type == TokenIf {
			elseIf, err := p.parseIfStmt()
			if err != nil {
				return nil, err
			}
			ifNode.Children = append(ifNode.Children, elseIf)
		} else {
			p.consume(TokenLBrace, "expect '{'")
			elseBody, err := p.parseStmtList()
			if err != nil {
				return nil, err
			}
			p.consume(TokenRBrace, "expect '}'")
			ifNode.Children = append(ifNode.Children, elseBody)
		}
	}

	return ifNode, nil
}

// parseWhileStmt parses a while statement
func (p *Parser) parseWhileStmt() (*ASTNode, error) {
	p.consume(TokenWhile, "expect 'while'")
	p.consume(TokenLParen, "expect '('")
	cond, err := p.parseExpr()
	if err != nil {
		return nil, err
	}
	p.consume(TokenRParen, "expect ')'")
	p.consume(TokenLBrace, "expect '{'")
	body, err := p.parseStmtList()
	if err != nil {
		return nil, err
	}
	p.consume(TokenRBrace, "expect '}'")

	return &ASTNode{
		Type:     NodeWhileStmt,
		Children: []*ASTNode{cond, body},
	}, nil
}

// parseForStmt parses a for statement
func (p *Parser) parseForStmt() (*ASTNode, error) {
	p.consume(TokenFor, "expect 'for'")
	p.consume(TokenLParen, "expect '('")
	var init *ASTNode
	if p.peek().Type != TokenSemicolon {
		var err error
		init, err = p.parseVarDecl()
		if err != nil {
			return nil, err
		}
	} else {
		p.consume(TokenSemicolon, "expect ';'")
	}
	var cond *ASTNode
	if p.peek().Type != TokenSemicolon {
		var err error
		cond, err = p.parseExpr()
		if err != nil {
			return nil, err
		}
	}
	p.consume(TokenSemicolon, "expect ';'")
	var update *ASTNode
	if p.peek().Type != TokenRParen {
		var err error
		// Parse update expression without expecting a semicolon
		ident := p.consume(TokenIdentifier, "expect identifier")
		var target *ASTNode
		if p.peek().Type == TokenLBracket {
			p.consume(TokenLBracket, "expect '['")
			index, err := p.parseExpr()
			if err != nil {
				return nil, err
			}
			p.consume(TokenRBracket, "expect ']'")
			target = &ASTNode{
				Type:     NodeArrayAccess,
				Children: []*ASTNode{{Type: NodeIdentifier, Value: ident.Literal, Token: ident}, index},
			}
		} else {
			target = &ASTNode{Type: NodeIdentifier, Value: ident.Literal, Token: ident}
		}
		p.consume(TokenEqual, "expect '='")
		expr, err := p.parseExpr()
		if err != nil {
			return nil, err
		}
		update = &ASTNode{
			Type:     NodeAssign,
			Children: []*ASTNode{target, expr},
		}
	}
	p.consume(TokenRParen, "expect ')'")
	p.consume(TokenLBrace, "expect '{'")
	body, err := p.parseStmtList()
	if err != nil {
		return nil, err
	}
	p.consume(TokenRBrace, "expect '}'")

	return &ASTNode{
		Type:     NodeForStmt,
		Children: []*ASTNode{init, cond, update, body},
	}, nil
}

// parseReturnStmt parses a return statement
func (p *Parser) parseReturnStmt() (*ASTNode, error) {
	returnTok := p.consume(TokenReturn, "expect 'return'")
	var expr *ASTNode
	if p.peek().Type != TokenSemicolon {
		var err error
		expr, err = p.parseExpr()
		if err != nil {
			return nil, err
		}
	}
	p.consume(TokenSemicolon, "expect ';'")
	return &ASTNode{
		Type:     NodeReturnStmt,
		Children: []*ASTNode{expr},
		Token:    returnTok,
	}, nil
}

// parsePrintStmt parses a print statement
func (p *Parser) parsePrintStmt() (*ASTNode, error) {
	p.consume(TokenPrint, "expect 'print'")
	p.consume(TokenLParen, "expect '('")
	expr, err := p.parseExpr()
	if err != nil {
		return nil, err
	}
	p.consume(TokenRParen, "expect ')'")
	p.consume(TokenSemicolon, "expect ';'")
	return &ASTNode{
		Type:     NodePrintStmt,
		Children: []*ASTNode{expr},
	}, nil
}

// parseAssign parses an assignment statement
func (p *Parser) parseAssign() (*ASTNode, error) {
	ident := p.consume(TokenIdentifier, "expect identifier")
	var target *ASTNode
	if p.peek().Type == TokenLBracket {
		p.consume(TokenLBracket, "expect '['")
		index, err := p.parseExpr()
		if err != nil {
			return nil, err
		}
		p.consume(TokenRBracket, "expect ']'")
		target = &ASTNode{
			Type:     NodeArrayAccess,
			Children: []*ASTNode{{Type: NodeIdentifier, Value: ident.Literal, Token: ident}, index},
		}
	} else {
		target = &ASTNode{Type: NodeIdentifier, Value: ident.Literal, Token: ident}
	}
	p.consume(TokenEqual, "expect '='")
	expr, err := p.parseExpr()
	if err != nil {
		return nil, err
	}
	p.consume(TokenSemicolon, "expect ';'")
	return &ASTNode{
		Type:     NodeAssign,
		Children: []*ASTNode{target, expr},
	}, nil
}

// parseExpr parses an expression
func (p *Parser) parseExpr() (*ASTNode, error) {
	return p.parseEquality()
}

// parseEquality parses equality expressions
func (p *Parser) parseEquality() (*ASTNode, error) {
	expr, err := p.parseComparison()
	if err != nil {
		return nil, err
	}
	if p.peek().Type == TokenDoubleEqual {
		op := p.consume(TokenDoubleEqual, "expect '=='")
		right, err := p.parseComparison()
		if err != nil {
			return nil, err
		}
		expr = &ASTNode{
			Type:     NodeBinaryExpr,
			Value:    op.Literal,
			Children: []*ASTNode{expr, right},
			Token:    op,
		}
	}
	return expr, nil
}

// parseComparison parses comparison expressions
func (p *Parser) parseComparison() (*ASTNode, error) {
	expr, err := p.parseAdditive()
	if err != nil {
		return nil, err
	}
	if p.peek().Type == TokenLessThan {
		op := p.consume(TokenLessThan, "expect '<'")
		right, err := p.parseAdditive()
		if err != nil {
			return nil, err
		}
		expr = &ASTNode{
			Type:     NodeBinaryExpr,
			Value:    op.Literal,
			Children: []*ASTNode{expr, right},
			Token:    op,
		}
	}
	return expr, nil
}

// parseAdditive parses additive expressions
func (p *Parser) parseAdditive() (*ASTNode, error) {
	expr, err := p.parseMultiplicative()
	if err != nil {
		return nil, err
	}
	for p.peek().Type == TokenPlus || p.peek().Type == TokenMinus {
		op := p.advance()
		right, err := p.parseMultiplicative()
		if err != nil {
			return nil, err
		}
		expr = &ASTNode{
			Type:     NodeBinaryExpr,
			Value:    op.Literal,
			Children: []*ASTNode{expr, right},
			Token:    op,
		}
	}
	return expr, nil
}

// parseMultiplicative parses multiplicative expressions
func (p *Parser) parseMultiplicative() (*ASTNode, error) {
	expr, err := p.parseUnary()
	if err != nil {
		return nil, err
	}
	for p.peek().Type == TokenMultiply || p.peek().Type == TokenDivide {
		op := p.advance()
		right, err := p.parseUnary()
		if err != nil {
			return nil, err
		}
		expr = &ASTNode{
			Type:     NodeBinaryExpr,
			Value:    op.Literal,
			Children: []*ASTNode{expr, right},
			Token:    op,
		}
	}
	return expr, nil
}

// parseUnary parses unary expressions
func (p *Parser) parseUnary() (*ASTNode, error) {
	if p.peek().Type == TokenBang {
		op := p.consume(TokenBang, "expect '!'")
		expr, err := p.parseUnary()
		if err != nil {
			return nil, err
		}
		return &ASTNode{
			Type:     NodeUnaryExpr,
			Value:    op.Literal,
			Children: []*ASTNode{expr},
			Token:    op,
		}, nil
	}
	return p.parsePrimary()
}

// parsePrimary parses primary expressions
func (p *Parser) parsePrimary() (*ASTNode, error) {
	switch p.peek().Type {
	case TokenNumber:
		num := p.advance()
		value, err := strconv.Atoi(num.Literal)
		if err != nil {
			return nil, fmt.Errorf("invalid number at line %d: %s", num.Line, num.Literal)
		}
		return &ASTNode{Type: NodeLiteral, Value: value, Token: num}, nil
	case TokenString:
		str := p.advance()
		return &ASTNode{Type: NodeLiteral, Value: str.Literal, Token: str}, nil
	case TokenTrue, TokenFalse:
		boolTok := p.advance()
		value := boolTok.Type == TokenTrue
		return &ASTNode{Type: NodeLiteral, Value: value, Token: boolTok}, nil
	case TokenIdentifier:
		ident := p.advance()
		if p.peek().Type == TokenLBracket {
			p.consume(TokenLBracket, "expect '['")
			index, err := p.parseExpr()
			if err != nil {
				return nil, err
			}
			p.consume(TokenRBracket, "expect ']'")
			return &ASTNode{
				Type:     NodeArrayAccess,
				Children: []*ASTNode{{Type: NodeIdentifier, Value: ident.Literal, Token: ident}, index},
			}, nil
		}
		if p.peek().Type == TokenDot && p.peekN(1).Type == TokenLength {
			p.consume(TokenDot, "expect '.'")
			lengthTok := p.consume(TokenLength, "expect 'length'")
			return &ASTNode{
				Type:     NodeArrayLength,
				Children: []*ASTNode{{Type: NodeIdentifier, Value: ident.Literal, Token: ident}},
				Token:    lengthTok,
			}, nil
		}
		return &ASTNode{Type: NodeIdentifier, Value: ident.Literal, Token: ident}, nil
	case TokenLParen:
		p.consume(TokenLParen, "expect '('")
		expr, err := p.parseExpr()
		if err != nil {
			return nil, err
		}
		p.consume(TokenRParen, "expect ')'")
		return expr, nil
	default:
		return nil, fmt.Errorf("unexpected token at line %d: %s", p.peek().Line, p.peek().Literal)
	}
}

// parseArrayLiteral parses an array literal
func (p *Parser) parseArrayLiteral() (*ASTNode, error) {
	p.consume(TokenLBracket, "expect '['")
	array := &ASTNode{Type: NodeArrayLiteral}
	for p.peek().Type != TokenRBracket {
		expr, err := p.parseExpr()
		if err != nil {
			return nil, err
		}
		array.Children = append(array.Children, expr)
		if p.peek().Type != TokenComma {
			break
		}
		p.consume(TokenComma, "expect ','")
	}
	p.consume(TokenRBracket, "expect ']'")
	return array, nil
}

// parseType parses a type specifier
func (p *Parser) parseType() *ASTNode {
	if p.peek().Type == TokenInt || p.peek().Type == TokenBool {
		tok := p.advance()
		return &ASTNode{Type: NodeLiteral, Value: tok.Literal, Token: tok}
	}
	panic(fmt.Sprintf("unexpected type token at line %d: %s", p.peek().Line, p.peek().Literal))
}

// consume checks and advances if the token matches
func (p *Parser) consume(tt TokenType, msg string) Token {
	if p.peek().Type != tt {
		panic(fmt.Sprintf("%s at line %d, found %s", msg, p.peek().Line, p.peek().Literal))
	}
	return p.advance()
}

// advance moves to the next token
func (p *Parser) advance() Token {
	if !p.isAtEnd() {
		p.current++
	}
	return p.tokens[p.current-1]
}

// peek returns the current token without advancing
func (p *Parser) peek() Token {
	return p.tokens[p.current]
}

// peekN peeks n tokens ahead
func (p *Parser) peekN(n int) Token {
	if p.current+n < len(p.tokens) {
		return p.tokens[p.current+n]
	}
	return Token{Type: TokenEOF}
}

// isAtEnd checks if parsing is complete
func (p *Parser) isAtEnd() bool {
	return p.current >= len(p.tokens) || p.peek().Type == TokenEOF
}
