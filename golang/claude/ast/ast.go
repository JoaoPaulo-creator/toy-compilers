// ast/ast.go
package ast

import "toycompiler/lexer"

// Node represents a node in the Abstract Syntax Tree
type Node interface {
	String() string
}

// Statement represents a statement node
type Statement interface {
	Node
	statementNode()
}

// Expression represents an expression node
type Expression interface {
	Node
	expressionNode()
}

// Program represents the root of the AST
type Program struct {
	Functions []*FunctionDeclaration
}

func (p *Program) String() string {
	return "Program"
}

// FunctionDeclaration represents a function declaration
type FunctionDeclaration struct {
	Name       string
	Parameters []string
	Body       *BlockStatement
}

func (fd *FunctionDeclaration) String() string {
	return "FunctionDeclaration: " + fd.Name
}

func (fd *FunctionDeclaration) statementNode() {}

// BlockStatement represents a block of statements
type BlockStatement struct {
	Statements []Statement
}

func (bs *BlockStatement) String() string {
	return "BlockStatement"
}

func (bs *BlockStatement) statementNode() {}

// VariableDeclaration represents a variable declaration
type VariableDeclaration struct {
	Type      string // "int", "bool"
	Name      string
	Value     Expression
	IsArray   bool
	ArraySize Expression // for array declarations
}

func (vd *VariableDeclaration) String() string {
	return "VariableDeclaration: " + vd.Name
}

func (vd *VariableDeclaration) statementNode() {}

// AssignmentStatement represents an assignment
type AssignmentStatement struct {
	Target Expression // can be Identifier or IndexExpression
	Value  Expression
}

func (as *AssignmentStatement) String() string {
	return "AssignmentStatement"
}

func (as *AssignmentStatement) statementNode() {}

// IfStatement represents an if statement
type IfStatement struct {
	Condition Expression
	ThenBody  *BlockStatement
	ElseBody  *BlockStatement // nil if no else clause
}

func (is *IfStatement) String() string {
	return "IfStatement"
}

func (is *IfStatement) statementNode() {}

// WhileStatement represents a while loop
type WhileStatement struct {
	Condition Expression
	Body      *BlockStatement
}

func (ws *WhileStatement) String() string {
	return "WhileStatement"
}

func (ws *WhileStatement) statementNode() {}

// ForStatement represents a for loop
type ForStatement struct {
	Init      Statement  // initialization
	Condition Expression // loop condition
	Update    Statement  // update statement
	Body      *BlockStatement
}

func (fs *ForStatement) String() string {
	return "ForStatement"
}

func (fs *ForStatement) statementNode() {}

// ReturnStatement represents a return statement
type ReturnStatement struct {
	Value Expression // nil for empty return
}

func (rs *ReturnStatement) String() string {
	return "ReturnStatement"
}

func (rs *ReturnStatement) statementNode() {}

// ExpressionStatement represents an expression used as a statement
type ExpressionStatement struct {
	Expression Expression
}

func (es *ExpressionStatement) String() string {
	return "ExpressionStatement"
}

func (es *ExpressionStatement) statementNode() {}

// Identifier represents an identifier
type Identifier struct {
	Value string
}

func (i *Identifier) String() string {
	return "Identifier: " + i.Value
}

func (i *Identifier) expressionNode() {}

// IntegerLiteral represents an integer literal
type IntegerLiteral struct {
	Value int
}

func (il *IntegerLiteral) String() string {
	return "IntegerLiteral"
}

func (il *IntegerLiteral) expressionNode() {}

// StringLiteral represents a string literal
type StringLiteral struct {
	Value string
}

func (sl *StringLiteral) String() string {
	return "StringLiteral"
}

func (sl *StringLiteral) expressionNode() {}

// BooleanLiteral represents a boolean literal
type BooleanLiteral struct {
	Value bool
}

func (bl *BooleanLiteral) String() string {
	return "BooleanLiteral"
}

func (bl *BooleanLiteral) expressionNode() {}

// ArrayLiteral represents an array literal [1, 2, 3]
type ArrayLiteral struct {
	Elements []Expression
}

func (al *ArrayLiteral) String() string {
	return "ArrayLiteral"
}

func (al *ArrayLiteral) expressionNode() {}

// BinaryExpression represents a binary operation
type BinaryExpression struct {
	Left     Expression
	Operator lexer.TokenType
	Right    Expression
}

func (be *BinaryExpression) String() string {
	return "BinaryExpression"
}

func (be *BinaryExpression) expressionNode() {}

// UnaryExpression represents a unary operation
type UnaryExpression struct {
	Operator lexer.TokenType
	Right    Expression
}

func (ue *UnaryExpression) String() string {
	return "UnaryExpression"
}

func (ue *UnaryExpression) expressionNode() {}

// CallExpression represents a function call
type CallExpression struct {
	Function  Expression
	Arguments []Expression
}

func (ce *CallExpression) String() string {
	return "CallExpression"
}

func (ce *CallExpression) expressionNode() {}

// IndexExpression represents array indexing arr[index]
type IndexExpression struct {
	Left  Expression // the array
	Index Expression // the index
}

func (ie *IndexExpression) String() string {
	return "IndexExpression"
}

func (ie *IndexExpression) expressionNode() {}

// DotExpression represents dot notation for builtin functions arr.length
type DotExpression struct {
	Left     Expression // the object
	Property string     // the property name
}

func (de *DotExpression) String() string {
	return "DotExpression"
}

func (de *DotExpression) expressionNode() {}
