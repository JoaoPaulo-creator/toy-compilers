package frontend

import (
	"fmt"
	"unicode"
)

// TokenType represents the type of a token
type TokenType int

const (
	// Keywords
	TokenFunc TokenType = iota
	TokenInc  TokenType = iota
	TokenDec
	TokenInt
	TokenBool
	TokenIf
	TokenElse
	TokenWhile
	TokenFor
	TokenReturn
	TokenPrint
	TokenLength

	// Symbols
	TokenLParen
	TokenRParen
	TokenLBrace
	TokenRBrace
	TokenLBracket
	TokenRBracket
	TokenSemicolon
	TokenComma
	TokenDot
	TokenEqual
	TokenPlus
	TokenMinus
	TokenMultiply
	TokenDivide
	TokenBang
	TokenDoubleEqual
	TokenLessThan

	// Literals
	TokenIdentifier
	TokenNumber
	TokenString
	TokenTrue
	TokenFalse

	// Special tokens
	TokenInvalid // Added TokenInvalid
	TokenEOF
)

// Token represents a lexical token
type Token struct {
	Type    TokenType
	Literal string
	Line    int
	Column  int
}

// Lexer holds the state for lexical analysis
type Lexer struct {
	input  string
	pos    int
	line   int
	column int
}

// NewLexer creates a new lexer
func NewLexer(input string) *Lexer {
	return &Lexer{
		input:  input,
		line:   1,
		column: 1,
	}
}

// Scan performs lexical analysis and returns tokens
func (l *Lexer) Scan() ([]Token, error) {
	var tokens []Token

	for {
		token := l.nextToken()
		tokens = append(tokens, token)
		if token.Type == TokenEOF {
			break
		}
		if token.Type == TokenInvalid {
			return nil, fmt.Errorf("invalid token at line %d, column %d: %s", token.Line, token.Column, token.Literal)
		}
	}

	return tokens, nil
}

// nextToken scans the next token
func (l *Lexer) nextToken() Token {
	l.skipWhitespace()

	if l.pos >= len(l.input) {
		return Token{Type: TokenEOF, Line: l.line, Column: l.column}
	}

	ch := l.input[l.pos]
	line, column := l.line, l.column

	switch {
	case ch == '(':
		l.advance()
		return Token{Type: TokenLParen, Literal: "(", Line: line, Column: column}
	case ch == ')':
		l.advance()
		return Token{Type: TokenRParen, Literal: ")", Line: line, Column: column}
	case ch == '{':
		l.advance()
		return Token{Type: TokenLBrace, Literal: "{", Line: line, Column: column}
	case ch == '}':
		l.advance()
		return Token{Type: TokenRBrace, Literal: "}", Line: line, Column: column}
	case ch == '[':
		l.advance()
		return Token{Type: TokenLBracket, Literal: "[", Line: line, Column: column}
	case ch == ']':
		l.advance()
		return Token{Type: TokenRBracket, Literal: "]", Line: line, Column: column}
	case ch == ';':
		l.advance()
		return Token{Type: TokenSemicolon, Literal: ";", Line: line, Column: column}
	case ch == ',':
		l.advance()
		return Token{Type: TokenComma, Literal: ",", Line: line, Column: column}
	case ch == '.':
		l.advance()
		return Token{Type: TokenDot, Literal: ".", Line: line, Column: column}
	case ch == '=':
		if l.peek() == '=' {
			l.advance()
			l.advance()
			return Token{Type: TokenDoubleEqual, Literal: "==", Line: line, Column: column}
		}
		l.advance()
		return Token{Type: TokenEqual, Literal: "=", Line: line, Column: column}
	case ch == '+':
		if l.peek() == '+' {
			l.advance()
			l.advance()
			return Token{Type: TokenInc, Literal: "++", Line: line, Column: column}
		}
		l.advance()
		return Token{Type: TokenPlus, Literal: "+", Line: line, Column: column}
	case ch == '-':
		if l.peek() == '-' {
			l.advance()
			l.advance()
			return Token{Type: TokenDec, Literal: "--", Line: line, Column: column}
		}
		l.advance()
		return Token{Type: TokenMinus, Literal: "-", Line: line, Column: column}
	case ch == '*':
		l.advance()
		return Token{Type: TokenMultiply, Literal: "*", Line: line, Column: column}
	case ch == '/':
		if l.peek() == '/' {
			// Handle single-line comments
			for l.pos < len(l.input) && l.input[l.pos] != '\n' {
				l.advance()
			}
			return l.nextToken()
		}
		l.advance()
		return Token{Type: TokenDivide, Literal: "/", Line: line, Column: column}
	case ch == '!':
		l.advance()
		return Token{Type: TokenBang, Literal: "!", Line: line, Column: column}
	case ch == '<':
		l.advance()
		return Token{Type: TokenLessThan, Literal: "<", Line: line, Column: column}
	case unicode.IsLetter(rune(ch)):
		ident := l.readIdentifier()
		tt := l.lookupKeyword(ident)
		return Token{Type: tt, Literal: ident, Line: line, Column: column}
	case unicode.IsDigit(rune(ch)):
		num := l.readNumber()
		return Token{Type: TokenNumber, Literal: num, Line: line, Column: column}
	case ch == '"':
		str, err := l.readString()
		if err != nil {
			return Token{Type: TokenInvalid, Literal: err.Error(), Line: line, Column: column}
		}
		return Token{Type: TokenString, Literal: str, Line: line, Column: column}
	default:
		l.advance()
		return Token{Type: TokenInvalid, Literal: string(ch), Line: line, Column: column}
	}
}

// skipWhitespace skips spaces, tabs, and newlines
func (l *Lexer) skipWhitespace() {
	for l.pos < len(l.input) && (l.input[l.pos] == ' ' || l.input[l.pos] == '\t' || l.input[l.pos] == '\n' || l.input[l.pos] == '\r') {
		if l.input[l.pos] == '\n' {
			l.line++
			l.column = 1
		} else {
			l.column++
		}
		l.pos++
	}
}

// advance moves to the next character
func (l *Lexer) advance() {
	l.pos++
	l.column++
}

// peek returns the next character without advancing
func (l *Lexer) peek() byte {
	if l.pos+1 < len(l.input) {
		return l.input[l.pos+1]
	}
	return 0
}

// readIdentifier reads an identifier or keyword
func (l *Lexer) readIdentifier() string {
	start := l.pos
	for l.pos < len(l.input) && (unicode.IsLetter(rune(l.input[l.pos])) || unicode.IsDigit(rune(l.input[l.pos])) || l.input[l.pos] == '_') {
		l.advance()
	}
	return l.input[start:l.pos]
}

// readNumber reads a number literal
func (l *Lexer) readNumber() string {
	start := l.pos
	for l.pos < len(l.input) && unicode.IsDigit(rune(l.input[l.pos])) {
		l.advance()
	}
	return l.input[start:l.pos]
}

// readString reads a string literal
func (l *Lexer) readString() (string, error) {
	l.advance() // Skip opening quote
	start := l.pos
	for l.pos < len(l.input) && l.input[l.pos] != '"' {
		if l.input[l.pos] == '\n' || l.input[l.pos] == '\r' {
			return "", fmt.Errorf("unterminated string")
		}
		l.advance()
	}
	if l.pos >= len(l.input) {
		return "", fmt.Errorf("unterminated string")
	}
	str := l.input[start:l.pos]
	l.advance() // Skip closing quote
	return str, nil
}

// lookupKeyword determines if an identifier is a keyword
func (l *Lexer) lookupKeyword(ident string) TokenType {
	keywords := map[string]TokenType{
		"func":   TokenFunc,
		"int":    TokenInt,
		"bool":   TokenBool,
		"if":     TokenIf,
		"else":   TokenElse,
		"while":  TokenWhile,
		"for":    TokenFor,
		"return": TokenReturn,
		"print":  TokenPrint,
		"length": TokenLength,
		"true":   TokenTrue,
		"false":  TokenFalse,
	}
	if tt, ok := keywords[ident]; ok {
		return tt
	}
	return TokenIdentifier
}
