package token

type TokenType string

type Token struct {
	Type    TokenType
	Literal string
}

const (
	EOF     = "EOF"
	ILLEGAL = "ILLEGAL"
	INT     = "INT"
	IDENT   = "IDENT"

	PLUS   = "+"
	ASSIGN = "="

	COMMA     = ","
	SEMICOLON = ";"

	LPAREN = "("
	RPAREN = ")"
	LBRACE = "{"
	RBRACE = "}"

	LET      = "LET"
	FUNCTION = "FUNCTION"
)
