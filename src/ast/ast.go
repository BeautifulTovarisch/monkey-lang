package ast

import (
	"monkey/token"
)

type Node interface {
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

// Return first token of statement or nothing
func (prog *Program) TokenLiteral() string {
	if len(prog.Statements) > 0 {
		return prog.Statements[0].TokenLiteral()
	}

	return ""
}

func (id *Identifier) expression_node()     {}
func (id *Identifier) TokenLiteral() string { return id.Token.Literal }

func (let *LetStatement) statement_node()      {}
func (let *LetStatement) TokenLiteral() string { return let.Token.Literal }

func (ret *ReturnStatement) statement_node()      {}
func (ret *ReturnStatement) TokenLiteral() string { return ret.Token.Literal }
