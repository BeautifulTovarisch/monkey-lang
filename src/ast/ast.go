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

type Boolean struct {
	Token token.Token
	Value bool
}

type Identifier struct {
	Token token.Token
	Value string
}

type BlockStatement struct {
	Token      token.Token
	Statements []Statement
}

type IfExpression struct {
	Token       token.Token
	Condition   Expression
	Alternative *BlockStatement
	Consequence *BlockStatement
}

type LetStatement struct {
	Name  *Identifier
	Token token.Token
	Value Expression
}

type IntegerLiteral struct {
	Token token.Token
	Value int64
}

type ReturnStatement struct {
	Token       token.Token
	ReturnValue Expression
}

type InfixExpression struct {
	Left     Expression
	Right    Expression
	Token    token.Token
	Operator string
}

type PrefixExpression struct {
	Right    Expression
	Token    token.Token
	Operator string
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

// Boolean
func (b *Boolean) expression_node()     {}
func (b *Boolean) String() string       { return b.Token.Literal }
func (b *Boolean) TokenLiteral() string { return b.Token.Literal }

// IfExpression
func (ife *IfExpression) expression_node() {}
func (ife *IfExpression) String() string {
	var out bytes.Buffer

	out.WriteString("if")
	out.WriteString(ife.Condition.String())
	out.WriteString(" ")
	out.WriteString(ife.Consequence.String())

	if ife.Alternative != nil {
		out.WriteString("else ")
		out.WriteString(ife.Alternative.String())
	}

	return out.String()
}
func (ife *IfExpression) TokenLiteral() string { return ife.Token.Literal }

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

//BlockStatement
func (bks *BlockStatement) statement_node() {}
func (bks *BlockStatement) String() string {
	var out bytes.Buffer

	for _, stmt := range bks.Statements {
		out.WriteString(stmt.String())
	}

	return out.String()
}
func (bks *BlockStatement) TokenLiteral() string { return bks.Token.Literal }

// IntegerLiteral
func (i_lit *IntegerLiteral) expression_node()     {}
func (i_lit *IntegerLiteral) String() string       { return i_lit.Token.Literal }
func (i_lit *IntegerLiteral) TokenLiteral() string { return i_lit.Token.Literal }

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

// InfixExpression
func (inf *InfixExpression) expression_node() {}
func (inf *InfixExpression) String() string {
	var out bytes.Buffer

	out.WriteString("(")
	out.WriteString(inf.Left.String())
	out.WriteString(" " + inf.Operator + " ")
	out.WriteString(inf.Right.String())
	out.WriteString(")")

	return out.String()
}
func (inf *InfixExpression) TokenLiteral() string { return inf.Token.Literal }

// PrefixExpression
func (pre *PrefixExpression) expression_node() {}
func (pre *PrefixExpression) String() string {
	var out bytes.Buffer

	out.WriteString("(")
	out.WriteString(pre.Operator)
	out.WriteString(pre.Right.String())
	out.WriteString(")")

	return out.String()
}
func (pre *PrefixExpression) TokenLiteral() string { return pre.Token.Literal }

// ExpressionStatement
func (ret *ExpressionStatement) statement_node() {}
func (ret *ExpressionStatement) String() string {
	if ret.Expression != nil {
		return ret.Expression.String()
	}

	return ""
}
func (ret *ExpressionStatement) TokenLiteral() string { return ret.Token.Literal }
