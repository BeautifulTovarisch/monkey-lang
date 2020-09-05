package token

const (
	EOF       = "EOF"
	INT       = "INT"
	LET       = "LET"
	PLUS      = "+"
	COMMA     = ","
	IDENT     = "IDENT"
	ASSIGN    = "="
	LBRACE    = "{"
	LPAREN    = "("
	RBRACE    = "}"
	RPAREN    = ")"
	ILLEGAL   = "ILLEGAL"
	FUNCTION  = "FUNCTION"
	SEMICOLON = ";"
)

type TokenType string

type Token struct {
	Type    TokenType
	Literal string
}

var keywords = map[string]TokenType{
	"fn":  FUNCTION,
	"let": LET,
}

// If token is a reserved word return it, otherwise consider it an identifier
func LookupIdent(ident string) TokenType {
	if tok, ok := keywords[ident]; ok {
		return tok
	}

	return IDENT
}
