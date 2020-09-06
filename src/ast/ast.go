package ast

import (
	"monkey/token"
)

type Node interface {
	TokenLiteral() string
}

type Identifier struct {
	Token token.Token
	Value string
}

type Statement interface {
	Node
	statement_node()
}

type Program struct {
	Statements []Statement
}

type Expression interface {
	Node
	expression_node()
}

type LetStatement struct {
	Token token.Token
	Name  *Identifier
	Value Expression
}

// Return first token of statement or nothing
func (prog *Program) TokenLiteral() string {
	if len(prog.Statements) > 0 {
		return prog.Statements[0].TokenLiteral()
	}

	return ""
}

func (let *LetStatement) statement_node()      {}
func (let *LetStatement) TokenLiteral() string { return let.Token.Literal }

func (id *Identifier) expression_node()     {}
func (id *Identifier) TokenLiteral() string { return id.Token.Literal }
