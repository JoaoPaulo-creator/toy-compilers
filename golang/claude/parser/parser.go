// parser/parser.go
package parser

import (
	"fmt"
	"strconv"

	"toycompiler/ast"
	"toycompiler/lexer"
)

// Parser represents the syntax analyzer
type Parser struct {
	tokens   []lexer.Token
	position int
	current  lexer.Token
}

// NewParser creates a new parser instance
func NewParser(tokens []lexer.Token) *Parser {
	p := &Parser{
		tokens:   tokens,
		position: 0,
	}
	if len(tokens) > 0 {
		p.current = tokens[0]
	}
	return p
}

// Parse performs syntax analysis on the tokens
func Parse(tokens []lexer.Token) (*ast.Program, error) {
	parser := NewParser(tokens)
	return parser.parseProgram()
}

// advance moves to the next token
func (p *Parser) advance() {
	if p.position < len(p.tokens)-1 {
		p.position++
		p.current = p.tokens[p.position]
	}
}

// peek returns the next token without advancing
func (p *Parser) peek() lexer.Token {
	if p.position+1 < len(p.tokens) {
		return p.tokens[p.position+1]
	}
	return lexer.Token{Type: lexer.EOF}
}

// expect checks if the current token matches the expected type and advances
func (p *Parser) expect(tokenType lexer.TokenType) error {
	if p.current.Type != tokenType {
		return fmt.Errorf("expected token %v, got %v at line %d, column %d",
			tokenType, p.current.Type, p.current.Line, p.current.Column)
	}
	p.advance()
	return nil
}

// parseProgram parses the entire program
func (p *Parser) parseProgram() (*ast.Program, error) {
	program := &ast.Program{}

	for p.current.Type != lexer.EOF {
		function, err := p.parseFunctionDeclaration()
		if err != nil {
			return nil, err
		}
		program.Functions = append(program.Functions, function)
	}

	return program, nil
}

// parseFunctionDeclaration parses a function declaration
func (p *Parser) parseFunctionDeclaration() (*ast.FunctionDeclaration, error) {
	// Parse 'func' keyword
	if err := p.expect(lexer.FUNC); err != nil {
		return nil, err
	}

	// Parse function name
	if p.current.Type != lexer.IDENTIFIER {
		return nil, fmt.Errorf("expected function name at line %d, column %d",
			p.current.Line, p.current.Column)
	}

	name := p.current.Value
	p.advance()

	// Parse parameter list
	if err := p.expect(lexer.LPAREN); err != nil {
		return nil, err
	}

	var parameters []string

	// Parse parameters if they exist
	if p.current.Type != lexer.RPAREN {
		for {
			if p.current.Type != lexer.IDENTIFIER {
				return nil, fmt.Errorf("expected parameter name at line %d, column %d",
					p.current.Line, p.current.Column)
			}

			parameters = append(parameters, p.current.Value)
			p.advance()

			if p.current.Type == lexer.COMMA {
				p.advance()
			} else {
				break
			}
		}
	}

	if err := p.expect(lexer.RPAREN); err != nil {
		return nil, err
	}

	// Parse function body
	body, err := p.parseBlockStatement()
	if err != nil {
		return nil, err
	}

	return &ast.FunctionDeclaration{
		Name:       name,
		Parameters: parameters,
		Body:       body,
	}, nil
}

// parseBlockStatement parses a block statement
func (p *Parser) parseBlockStatement() (*ast.BlockStatement, error) {
	if err := p.expect(lexer.LBRACE); err != nil {
		return nil, err
	}

	var statements []ast.Statement

	for p.current.Type != lexer.RBRACE && p.current.Type != lexer.EOF {
		stmt, err := p.parseStatement()
		if err != nil {
			return nil, err
		}
		statements = append(statements, stmt)
	}

	if err := p.expect(lexer.RBRACE); err != nil {
		return nil, err
	}

	return &ast.BlockStatement{
		Statements: statements,
	}, nil
}

// parseStatement parses a statement
func (p *Parser) parseStatement() (ast.Statement, error) {
	switch p.current.Type {
	case lexer.INT, lexer.BOOL:
		return p.parseVariableDeclaration()
	case lexer.IF:
		return p.parseIfStatement()
	case lexer.WHILE:
		return p.parseWhileStatement()
	case lexer.FOR:
		return p.parseForStatement()
	case lexer.RETURN:
		return p.parseReturnStatement()
	case lexer.IDENTIFIER:
		// Could be assignment or expression statement
		return p.parseAssignmentOrExpressionStatement()
	default:
		// Try to parse as expression statement
		return p.parseExpressionStatement()
	}
}

// parseVariableDeclaration parses a variable declaration
func (p *Parser) parseVariableDeclaration() (*ast.VariableDeclaration, error) {
	// Parse type
	varType := p.current.Value
	p.advance()

	// Parse variable name
	if p.current.Type != lexer.IDENTIFIER {
		return nil, fmt.Errorf("expected variable name at line %d, column %d",
			p.current.Line, p.current.Column)
	}

	name := p.current.Value
	p.advance()

	// Check for array declaration
	isArray := false
	if p.current.Type == lexer.LBRACKET {
		isArray = true
		p.advance()
		if err := p.expect(lexer.RBRACKET); err != nil {
			return nil, err
		}
	}

	// Parse assignment
	if err := p.expect(lexer.ASSIGN); err != nil {
		return nil, err
	}

	value, err := p.parseExpression()
	if err != nil {
		return nil, err
	}

	if err := p.expect(lexer.SEMICOLON); err != nil {
		return nil, err
	}

	return &ast.VariableDeclaration{
		Type:    varType,
		Name:    name,
		Value:   value,
		IsArray: isArray,
	}, nil
}

// parseIfStatement parses an if statement
func (p *Parser) parseIfStatement() (*ast.IfStatement, error) {
	if err := p.expect(lexer.IF); err != nil {
		return nil, err
	}

	if err := p.expect(lexer.LPAREN); err != nil {
		return nil, err
	}

	condition, err := p.parseExpression()
	if err != nil {
		return nil, err
	}

	if err := p.expect(lexer.RPAREN); err != nil {
		return nil, err
	}

	thenBody, err := p.parseBlockStatement()
	if err != nil {
		return nil, err
	}

	var elseBody *ast.BlockStatement
	if p.current.Type == lexer.ELSE {
		p.advance()

		// Check for else if
		if p.current.Type == lexer.IF {
			// Convert else if to nested if statement
			nestedIf, err := p.parseIfStatement()
			if err != nil {
				return nil, err
			}
			elseBody = &ast.BlockStatement{
				Statements: []ast.Statement{nestedIf},
			}
		} else {
			// Regular else block
			elseBody, err = p.parseBlockStatement()
			if err != nil {
				return nil, err
			}
		}
	}

	return &ast.IfStatement{
		Condition: condition,
		ThenBody:  thenBody,
		ElseBody:  elseBody,
	}, nil
}

// parseWhileStatement parses a while statement
func (p *Parser) parseWhileStatement() (*ast.WhileStatement, error) {
	if err := p.expect(lexer.WHILE); err != nil {
		return nil, err
	}

	if err := p.expect(lexer.LPAREN); err != nil {
		return nil, err
	}

	condition, err := p.parseExpression()
	if err != nil {
		return nil, err
	}

	if err := p.expect(lexer.RPAREN); err != nil {
		return nil, err
	}

	body, err := p.parseBlockStatement()
	if err != nil {
		return nil, err
	}

	return &ast.WhileStatement{
		Condition: condition,
		Body:      body,
	}, nil
}

// parseForStatement parses a for statement
func (p *Parser) parseForStatement() (*ast.ForStatement, error) {
	if err := p.expect(lexer.FOR); err != nil {
		return nil, err
	}

	if err := p.expect(lexer.LPAREN); err != nil {
		return nil, err
	}

	// Parse initialization
	init, err := p.parseStatement()
	if err != nil {
		return nil, err
	}

	// Parse condition
	condition, err := p.parseExpression()
	if err != nil {
		return nil, err
	}

	if err := p.expect(lexer.SEMICOLON); err != nil {
		return nil, err
	}

	// Parse update
	update, err := p.parseAssignmentOrExpressionStatement()
	if err != nil {
		return nil, err
	}

	if err := p.expect(lexer.RPAREN); err != nil {
		return nil, err
	}

	// Parse body
	body, err := p.parseBlockStatement()
	if err != nil {
		return nil, err
	}

	return &ast.ForStatement{
		Init:      init,
		Condition: condition,
		Update:    update,
		Body:      body,
	}, nil
}

// parseReturnStatement parses a return statement
func (p *Parser) parseReturnStatement() (*ast.ReturnStatement, error) {
	if err := p.expect(lexer.RETURN); err != nil {
		return nil, err
	}

	var value ast.Expression

	// Check if there's a return value
	if p.current.Type != lexer.SEMICOLON {
		var err error
		value, err = p.parseExpression()
		if err != nil {
			return nil, err
		}
	}

	if err := p.expect(lexer.SEMICOLON); err != nil {
		return nil, err
	}

	return &ast.ReturnStatement{
		Value: value,
	}, nil
}

// parseAssignmentOrExpressionStatement parses assignment or expression statement
func (p *Parser) parseAssignmentOrExpressionStatement() (ast.Statement, error) {
	// Parse the left side as an expression first
	expr, err := p.parseExpression()
	if err != nil {
		return nil, err
	}

	// Check if this is an assignment
	if p.current.Type == lexer.ASSIGN {
		p.advance()

		value, err := p.parseExpression()
		if err != nil {
			return nil, err
		}

		if err := p.expect(lexer.SEMICOLON); err != nil {
			return nil, err
		}

		return &ast.AssignmentStatement{
			Target: expr,
			Value:  value,
		}, nil
	}

	// Otherwise, it's an expression statement
	if err := p.expect(lexer.SEMICOLON); err != nil {
		return nil, err
	}

	return &ast.ExpressionStatement{
		Expression: expr,
	}, nil
}

// parseExpressionStatement parses an expression statement
func (p *Parser) parseExpressionStatement() (*ast.ExpressionStatement, error) {
	expr, err := p.parseExpression()
	if err != nil {
		return nil, err
	}

	if err := p.expect(lexer.SEMICOLON); err != nil {
		return nil, err
	}

	return &ast.ExpressionStatement{
		Expression: expr,
	}, nil
}

// parseExpression parses an expression using precedence climbing
func (p *Parser) parseExpression() (ast.Expression, error) {
	return p.parseLogicalOr()
}

// parseLogicalOr parses logical OR expressions
func (p *Parser) parseLogicalOr() (ast.Expression, error) {
	left, err := p.parseLogicalAnd()
	if err != nil {
		return nil, err
	}

	for p.current.Type == lexer.LOGICAL_OR {
		operator := p.current.Type
		p.advance()

		right, err := p.parseLogicalAnd()
		if err != nil {
			return nil, err
		}

		left = &ast.BinaryExpression{
			Left:     left,
			Operator: operator,
			Right:    right,
		}
	}

	return left, nil
}

// parseLogicalAnd parses logical AND expressions
func (p *Parser) parseLogicalAnd() (ast.Expression, error) {
	left, err := p.parseEquality()
	if err != nil {
		return nil, err
	}

	for p.current.Type == lexer.LOGICAL_AND {
		operator := p.current.Type
		p.advance()

		right, err := p.parseEquality()
		if err != nil {
			return nil, err
		}

		left = &ast.BinaryExpression{
			Left:     left,
			Operator: operator,
			Right:    right,
		}
	}

	return left, nil
}

// parseEquality parses equality expressions
func (p *Parser) parseEquality() (ast.Expression, error) {
	left, err := p.parseComparison()
	if err != nil {
		return nil, err
	}

	for p.current.Type == lexer.EQUAL || p.current.Type == lexer.NOT_EQUAL {
		operator := p.current.Type
		p.advance()

		right, err := p.parseComparison()
		if err != nil {
			return nil, err
		}

		left = &ast.BinaryExpression{
			Left:     left,
			Operator: operator,
			Right:    right,
		}
	}

	return left, nil
}

// parseComparison parses comparison expressions
func (p *Parser) parseComparison() (ast.Expression, error) {
	left, err := p.parseTerm()
	if err != nil {
		return nil, err
	}

	for p.current.Type == lexer.LESS_THAN || p.current.Type == lexer.GREATER_THAN ||
		p.current.Type == lexer.LESS_EQUAL || p.current.Type == lexer.GREATER_EQUAL {
		operator := p.current.Type
		p.advance()

		right, err := p.parseTerm()
		if err != nil {
			return nil, err
		}

		left = &ast.BinaryExpression{
			Left:     left,
			Operator: operator,
			Right:    right,
		}
	}

	return left, nil
}

// parseTerm parses addition and subtraction
func (p *Parser) parseTerm() (ast.Expression, error) {
	left, err := p.parseFactor()
	if err != nil {
		return nil, err
	}

	for p.current.Type == lexer.PLUS || p.current.Type == lexer.MINUS {
		operator := p.current.Type
		p.advance()

		right, err := p.parseFactor()
		if err != nil {
			return nil, err
		}

		left = &ast.BinaryExpression{
			Left:     left,
			Operator: operator,
			Right:    right,
		}
	}

	return left, nil
}

// parseFactor parses multiplication and division
func (p *Parser) parseFactor() (ast.Expression, error) {
	left, err := p.parseUnary()
	if err != nil {
		return nil, err
	}

	for p.current.Type == lexer.MULTIPLY || p.current.Type == lexer.DIVIDE {
		operator := p.current.Type
		p.advance()

		right, err := p.parseUnary()
		if err != nil {
			return nil, err
		}

		left = &ast.BinaryExpression{
			Left:     left,
			Operator: operator,
			Right:    right,
		}
	}

	return left, nil
}

// parseUnary parses unary expressions
func (p *Parser) parseUnary() (ast.Expression, error) {
	if p.current.Type == lexer.LOGICAL_NOT || p.current.Type == lexer.MINUS {
		operator := p.current.Type
		p.advance()

		right, err := p.parseUnary()
		if err != nil {
			return nil, err
		}

		return &ast.UnaryExpression{
			Operator: operator,
			Right:    right,
		}, nil
	}

	return p.parsePostfix()
}

// parsePostfix parses postfix expressions (function calls, array indexing, dot notation)
func (p *Parser) parsePostfix() (ast.Expression, error) {
	expr, err := p.parsePrimary()
	if err != nil {
		return nil, err
	}

	for {
		switch p.current.Type {
		case lexer.LPAREN:
			// Function call
			p.advance()

			var args []ast.Expression

			if p.current.Type != lexer.RPAREN {
				for {
					arg, err := p.parseExpression()
					if err != nil {
						return nil, err
					}
					args = append(args, arg)

					if p.current.Type == lexer.COMMA {
						p.advance()
					} else {
						break
					}
				}
			}

			if err := p.expect(lexer.RPAREN); err != nil {
				return nil, err
			}

			expr = &ast.CallExpression{
				Function:  expr,
				Arguments: args,
			}

		case lexer.LBRACKET:
			// Array indexing
			p.advance()

			index, err := p.parseExpression()
			if err != nil {
				return nil, err
			}

			if err := p.expect(lexer.RBRACKET); err != nil {
				return nil, err
			}

			expr = &ast.IndexExpression{
				Left:  expr,
				Index: index,
			}

		case lexer.DOT:
			// Dot notation for builtin functions
			p.advance()

			if p.current.Type != lexer.IDENTIFIER {
				return nil, fmt.Errorf("expected property name after '.' at line %d, column %d",
					p.current.Line, p.current.Column)
			}

			property := p.current.Value
			p.advance()

			expr = &ast.DotExpression{
				Left:     expr,
				Property: property,
			}

		default:
			return expr, nil
		}
	}
}

// parsePrimary parses primary expressions
func (p *Parser) parsePrimary() (ast.Expression, error) {
	switch p.current.Type {
	case lexer.IDENTIFIER:
		value := p.current.Value
		p.advance()
		return &ast.Identifier{Value: value}, nil

	case lexer.INTEGER:
		value, err := strconv.Atoi(p.current.Value)
		if err != nil {
			return nil, fmt.Errorf("invalid integer: %s", p.current.Value)
		}
		p.advance()
		return &ast.IntegerLiteral{Value: value}, nil

	case lexer.STRING:
		value := p.current.Value
		p.advance()
		return &ast.StringLiteral{Value: value}, nil

	case lexer.BOOLEAN:
		value := p.current.Value == "true"
		p.advance()
		return &ast.BooleanLiteral{Value: value}, nil

	case lexer.LBRACKET:
		// Array literal
		p.advance()

		var elements []ast.Expression

		if p.current.Type != lexer.RBRACKET {
			for {
				element, err := p.parseExpression()
				if err != nil {
					return nil, err
				}
				elements = append(elements, element)

				if p.current.Type == lexer.COMMA {
					p.advance()
				} else {
					break
				}
			}
		}

		if err := p.expect(lexer.RBRACKET); err != nil {
			return nil, err
		}

		return &ast.ArrayLiteral{Elements: elements}, nil

	case lexer.LPAREN:
		// Grouped expression
		p.advance()

		expr, err := p.parseExpression()
		if err != nil {
			return nil, err
		}

		if err := p.expect(lexer.RPAREN); err != nil {
			return nil, err
		}

		return expr, nil

	default:
		return nil, fmt.Errorf("unexpected token %v at line %d, column %d",
			p.current.Type, p.current.Line, p.current.Column)
	}
}
