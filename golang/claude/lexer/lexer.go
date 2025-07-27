// lexer/lexer.go
package lexer

import (
	"fmt"
	"strconv"
	"unicode"
)

// TokenType represents the type of a token
type TokenType int

const (
	// Literals
	IDENTIFIER TokenType = iota
	INTEGER
	STRING
	BOOLEAN

	// Keywords
	FUNC
	IF
	ELSE
	WHILE
	FOR
	RETURN
	INT
	BOOL

	// Operators
	ASSIGN
	PLUS
	MINUS
	MULTIPLY
	DIVIDE
	EQUAL
	NOT_EQUAL
	LESS_THAN
	GREATER_THAN
	LESS_EQUAL
	GREATER_EQUAL
	LOGICAL_AND
	LOGICAL_OR
	LOGICAL_NOT

	// Delimiters
	SEMICOLON
	COMMA
	DOT
	LPAREN
	RPAREN
	LBRACE
	RBRACE
	LBRACKET
	RBRACKET

	// Special
	EOF
	NEWLINE
	COMMENT
)

// Token represents a lexical token
type Token struct {
	Type     TokenType
	Value    string
	Line     int
	Column   int
	Position int
}

// String returns a string representation of the token
func (t Token) String() string {
	return fmt.Sprintf("Token{Type: %v, Value: %q, Line: %d, Column: %d}", t.Type, t.Value, t.Line, t.Column)
}

// Lexer represents the lexical analyzer
type Lexer struct {
	input    string
	position int
	line     int
	column   int
}

// keywords maps keyword strings to their token types
var keywords = map[string]TokenType{
	"func":   FUNC,
	"if":     IF,
	"else":   ELSE,
	"while":  WHILE,
	"for":    FOR,
	"return": RETURN,
	"int":    INT,
	"bool":   BOOL,
	"true":   BOOLEAN,
	"false":  BOOLEAN,
}

// NewLexer creates a new lexer instance
func NewLexer(input string) *Lexer {
	return &Lexer{
		input:  input,
		line:   1,
		column: 1,
	}
}

// Tokenize performs lexical analysis on the input string
func Tokenize(input string) ([]Token, error) {
	lexer := NewLexer(input)
	var tokens []Token

	for {
		token, err := lexer.nextToken()
		if err != nil {
			return nil, err
		}

		// Skip comments and newlines
		if token.Type == COMMENT || token.Type == NEWLINE {
			continue
		}

		tokens = append(tokens, token)

		if token.Type == EOF {
			break
		}
	}

	return tokens, nil
}

// nextToken returns the next token from the input
func (l *Lexer) nextToken() (Token, error) {
	l.skipWhitespace()

	if l.position >= len(l.input) {
		return Token{Type: EOF, Line: l.line, Column: l.column, Position: l.position}, nil
	}
	char := rune(l.input[l.position])
	tokenStart := l.position
	lineStart := l.line
	columnStart := l.column

	switch char {
	case '\n':
		l.advance()
		return Token{Type: NEWLINE, Value: "\n", Line: lineStart, Column: columnStart, Position: tokenStart}, nil

	case ';':
		l.advance()
		return Token{Type: SEMICOLON, Value: ";", Line: lineStart, Column: columnStart, Position: tokenStart}, nil

	case ',':
		l.advance()
		return Token{Type: COMMA, Value: ",", Line: lineStart, Column: columnStart, Position: tokenStart}, nil

	case '.':
		l.advance()
		return Token{Type: DOT, Value: ".", Line: lineStart, Column: columnStart, Position: tokenStart}, nil

	case '(':
		l.advance()
		return Token{Type: LPAREN, Value: "(", Line: lineStart, Column: columnStart, Position: tokenStart}, nil

	case ')':
		l.advance()
		return Token{Type: RPAREN, Value: ")", Line: lineStart, Column: columnStart, Position: tokenStart}, nil

	case '{':
		l.advance()
		return Token{Type: LBRACE, Value: "{", Line: lineStart, Column: columnStart, Position: tokenStart}, nil

	case '}':
		l.advance()
		return Token{Type: RBRACE, Value: "}", Line: lineStart, Column: columnStart, Position: tokenStart}, nil

	case '[':
		l.advance()
		return Token{Type: LBRACKET, Value: "[", Line: lineStart, Column: columnStart, Position: tokenStart}, nil

	case ']':
		l.advance()
		return Token{Type: RBRACKET, Value: "]", Line: lineStart, Column: columnStart, Position: tokenStart}, nil

	case '+':
		l.advance()
		return Token{Type: PLUS, Value: "+", Line: lineStart, Column: columnStart, Position: tokenStart}, nil

	case '-':
		l.advance()
		return Token{Type: MINUS, Value: "-", Line: lineStart, Column: columnStart, Position: tokenStart}, nil

	case '*':
		l.advance()
		return Token{Type: MULTIPLY, Value: "*", Line: lineStart, Column: columnStart, Position: tokenStart}, nil

	case '/':
		// Check for comments
		if l.position+1 < len(l.input) && l.input[l.position+1] == '/' {
			return l.readComment()
		}
		l.advance()
		return Token{Type: DIVIDE, Value: "/", Line: lineStart, Column: columnStart, Position: tokenStart}, nil

	case '=':
		if l.position+1 < len(l.input) && l.input[l.position+1] == '=' {
			l.advance()
			l.advance()
			return Token{Type: EQUAL, Value: "==", Line: lineStart, Column: columnStart, Position: tokenStart}, nil
		}
		l.advance()
		return Token{Type: ASSIGN, Value: "=", Line: lineStart, Column: columnStart, Position: tokenStart}, nil

	case '!':
		if l.position+1 < len(l.input) && l.input[l.position+1] == '=' {
			l.advance()
			l.advance()
			return Token{Type: NOT_EQUAL, Value: "!=", Line: lineStart, Column: columnStart, Position: tokenStart}, nil
		}
		l.advance()
		return Token{Type: LOGICAL_NOT, Value: "!", Line: lineStart, Column: columnStart, Position: tokenStart}, nil

	case '<':
		if l.position+1 < len(l.input) && l.input[l.position+1] == '=' {
			l.advance()
			l.advance()
			return Token{Type: LESS_EQUAL, Value: "<=", Line: lineStart, Column: columnStart, Position: tokenStart}, nil
		}
		l.advance()
		return Token{Type: LESS_THAN, Value: "<", Line: lineStart, Column: columnStart, Position: tokenStart}, nil

	case '>':
		if l.position+1 < len(l.input) && l.input[l.position+1] == '=' {
			l.advance()
			l.advance()
			return Token{Type: GREATER_EQUAL, Value: ">=", Line: lineStart, Column: columnStart, Position: tokenStart}, nil
		}
		l.advance()
		return Token{Type: GREATER_THAN, Value: ">", Line: lineStart, Column: columnStart, Position: tokenStart}, nil

	case '&':
		if l.position+1 < len(l.input) && l.input[l.position+1] == '&' {
			l.advance()
			l.advance()
			return Token{Type: LOGICAL_AND, Value: "&&", Line: lineStart, Column: columnStart, Position: tokenStart}, nil
		}
		return Token{}, fmt.Errorf("unexpected character '&' at line %d, column %d", l.line, l.column)

	case '|':
		if l.position+1 < len(l.input) && l.input[l.position+1] == '|' {
			l.advance()
			l.advance()
			return Token{Type: LOGICAL_OR, Value: "||", Line: lineStart, Column: columnStart, Position: tokenStart}, nil
		}
		return Token{}, fmt.Errorf("unexpected character '|' at line %d, column %d", l.line, l.column)

	case '"':
		return l.readString()

	default:
		if unicode.IsLetter(char) || char == '_' {
			return l.readIdentifier()
		}
		if unicode.IsDigit(char) {
			return l.readInteger()
		}
		return Token{}, fmt.Errorf("unexpected character '%c' at line %d, column %d", char, l.line, l.column)
	}
}

// advance moves to the next character in the input
func (l *Lexer) advance() {
	if l.position < len(l.input) && l.input[l.position] == '\n' {
		l.line++
		l.column = 1
	} else {
		l.column++
	}
	l.position++
}

// skipWhitespace skips whitespace characters except newlines
func (l *Lexer) skipWhitespace() {
	for l.position < len(l.input) {
		char := l.input[l.position]
		if char == ' ' || char == '\t' || char == '\r' {
			l.advance()
		} else {
			break
		}
	}
}

// readIdentifier reads an identifier or keyword
func (l *Lexer) readIdentifier() (Token, error) {
	start := l.position
	lineStart := l.line
	columnStart := l.column

	for l.position < len(l.input) {
		char := rune(l.input[l.position])
		if unicode.IsLetter(char) || unicode.IsDigit(char) || char == '_' {
			l.advance()
		} else {
			break
		}
	}

	value := l.input[start:l.position]

	// Check if it's a keyword
	if tokenType, isKeyword := keywords[value]; isKeyword {
		return Token{Type: tokenType, Value: value, Line: lineStart, Column: columnStart, Position: start}, nil
	}

	return Token{Type: IDENTIFIER, Value: value, Line: lineStart, Column: columnStart, Position: start}, nil
}

// readInteger reads an integer literal
func (l *Lexer) readInteger() (Token, error) {
	start := l.position
	lineStart := l.line
	columnStart := l.column

	for l.position < len(l.input) && unicode.IsDigit(rune(l.input[l.position])) {
		l.advance()
	}

	value := l.input[start:l.position]

	// Validate the integer
	if _, err := strconv.Atoi(value); err != nil {
		return Token{}, fmt.Errorf("invalid integer '%s' at line %d, column %d", value, lineStart, columnStart)
	}

	return Token{Type: INTEGER, Value: value, Line: lineStart, Column: columnStart, Position: start}, nil
}

// readString reads a string literal
func (l *Lexer) readString() (Token, error) {
	start := l.position
	lineStart := l.line
	columnStart := l.column

	l.advance() // Skip opening quote

	for l.position < len(l.input) && l.input[l.position] != '"' {
		if l.input[l.position] == '\n' {
			return Token{}, fmt.Errorf("unterminated string at line %d, column %d", lineStart, columnStart)
		}
		l.advance()
	}

	if l.position >= len(l.input) {
		return Token{}, fmt.Errorf("unterminated string at line %d, column %d", lineStart, columnStart)
	}

	value := l.input[start+1 : l.position] // Exclude quotes
	l.advance()                            // Skip closing quote

	return Token{Type: STRING, Value: value, Line: lineStart, Column: columnStart, Position: start}, nil
}

// readComment reads a single-line comment
func (l *Lexer) readComment() (Token, error) {
	start := l.position
	lineStart := l.line
	columnStart := l.column

	// Skip the '//' characters
	l.advance()
	l.advance()

	// Read until end of line or end of input
	for l.position < len(l.input) && l.input[l.position] != '\n' {
		l.advance()
	}

	value := l.input[start:l.position]
	return Token{Type: COMMENT, Value: value, Line: lineStart, Column: columnStart, Position: start}, nil
}
