package lexer

import (
	"monkey/token"
)

type Lexer struct {
	ch            byte
	input         string
	position      int
	read_position int
}

func (lex *Lexer) read_char() {
	if lex.read_position >= len(lex.input) {
		lex.ch = 0
	} else {
		lex.ch = lex.input[lex.read_position]
	}

	lex.position = lex.read_position
	lex.read_position += 1
}

func new_token(token_type token.TokenType, ch byte) token.Token {
	return token.Token{Type: token_type, Literal: string(ch)}
}

// Exported

func New(input string) *Lexer {
	lex := &Lexer{input: input}
	lex.read_char()

	return lex
}

func (lex *Lexer) NextToken() token.Token {
	var tok token.Token

	switch lex.ch {
	case 0:
		tok = token.Token{Type: token.EOF, Literal: ""}
	case '=':
		tok = new_token(token.ASSIGN, lex.ch)
	case ';':
		tok = new_token(token.SEMICOLON, lex.ch)
	case '(':
		tok = new_token(token.LPAREN, lex.ch)
	case ')':
		tok = new_token(token.RPAREN, lex.ch)
	case ',':
		tok = new_token(token.COMMA, lex.ch)
	case '+':
		tok = new_token(token.PLUS, lex.ch)
	case '{':
		tok = new_token(token.LBRACE, lex.ch)
	case '}':
		tok = new_token(token.RBRACE, lex.ch)
	}

	lex.read_char()

	return tok
}
