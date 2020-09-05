package token

const (
	IF        = "IF"
	LT        = "<"
	GT        = ">"
	EOF       = "EOF"
	INT       = "INT"
	LET       = "LET"
	BANG      = "!"
	ELSE      = "ELSE"
	PLUS      = "+"
	TRUE      = "TRUE"
	COMMA     = ","
	FALSE     = "FALSE"
	IDENT     = "IDENT"
	MINUS     = "-"
	SLASH     = "/"
	ASSIGN    = "="
	LBRACE    = "{"
	LPAREN    = "("
	RBRACE    = "}"
	RPAREN    = ")"
	RETURN    = "RETURN"
	ILLEGAL   = "ILLEGAL"
	ASTERISK  = "*"
	FUNCTION  = "FUNCTION"
	SEMICOLON = ";"
)

type TokenType string

type Token struct {
	Type    TokenType
	Literal string
}

// TODO :: Consider moving this inside LookupIdent
var keywords = map[string]TokenType{
	"fn":     FUNCTION,
	"if":     IF,
	"let":    LET,
	"else":   ELSE,
	"true":   TRUE,
	"false":  FALSE,
	"return": RETURN,
}

// If token is a reserved word return it, otherwise consider it an identifier
func LookupIdent(ident string) TokenType {
	if tok, ok := keywords[ident]; ok {
		return tok
	}

	return IDENT
}
