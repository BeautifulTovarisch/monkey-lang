package ast

import (
	"bytes"

	"monkey/token"
)

type Node interface {
	String() string
	TokenLiteral() string
}

type Statement interface {
	Node
	statement_node()
}

type Expression interface {
	Node
	expression_node()
}

// Root of AST
type Program struct {
	Statements []Statement
}

type Identifier struct {
	Token token.Token
	Value string
}

type LetStatement struct {
	Token token.Token
	Name  *Identifier
	Value Expression
}

type ReturnStatement struct {
	Token       token.Token
	ReturnValue Expression
}

type ExpressionStatement struct {
	Token      token.Token
	Expression Expression
}

/* Implement String() method for all nodes
* Allows returning structure of program as a string for easy debugging
 */
func (prog *Program) String() string {
	var out bytes.Buffer

	for _, stmt := range prog.Statements {
		out.WriteString(stmt.String())
	}

	return out.String()
}

// Return first token of statement or nothing
func (prog *Program) TokenLiteral() string {
	if len(prog.Statements) > 0 {
		return prog.Statements[0].TokenLiteral()
	}

	return ""
}

// Identifier
func (id *Identifier) expression_node()     {}
func (id *Identifier) String() string       { return id.Value }
func (id *Identifier) TokenLiteral() string { return id.Token.Literal }

// LetStatement
func (let *LetStatement) statement_node() {}
func (let *LetStatement) String() string {
	var out bytes.Buffer

	out.WriteString(let.TokenLiteral() + " ")
	out.WriteString(let.Name.String())
	out.WriteString(" = ")

	if let.Value != nil {
		out.WriteString(let.Value.String())
	}

	out.WriteString(";")

	return out.String()
}
func (let *LetStatement) TokenLiteral() string { return let.Token.Literal }

// ReturnStatement
func (ret *ReturnStatement) statement_node() {}
func (ret *ReturnStatement) String() string {
	var out bytes.Buffer

	out.WriteString(ret.TokenLiteral() + " ")

	if ret.ReturnValue != nil {
		out.WriteString(ret.ReturnValue.String())
	}

	out.WriteString(";")

	return out.String()
}
func (ret *ReturnStatement) TokenLiteral() string { return ret.Token.Literal }

// ExpressionStatement
func (ret *ExpressionStatement) statement_node() {}
func (ret *ExpressionStatement) String() string {
	if ret.Expression != nil {
		return ret.Expression.String()
	}

	return ""
}
func (ret *ExpressionStatement) TokenLiteral() string { return ret.Token.Literal }
