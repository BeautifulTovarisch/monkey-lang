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

func new_token(token_type token.TokenType, ch byte) token.Token {
	return token.Token{Type: token_type, Literal: string(ch)}
}

func is_digit(ch byte) bool {
	return '0' <= ch && ch <= '9'
}

// Test if ascii letter
// TODO :: Allow unicode in identifiers
func is_letter(ch byte) bool {
	return 'a' <= ch && ch <= 'z' || 'A' <= ch && ch <= 'Z' || ch == '_'
}

// Read character from input. Null(0) character indicates EOF
func (lex *Lexer) read_char() {
	if lex.read_position >= len(lex.input) {
		lex.ch = 0
	} else {
		lex.ch = lex.input[lex.read_position]
	}

	lex.position = lex.read_position
	lex.read_position += 1
}

// Peek ahead without incrementing stream
func (lex *Lexer) peek_char() byte {
	if lex.read_position >= len(lex.input) {
		return 0
	}

	return lex.input[lex.read_position]
}

func (lex *Lexer) read_number() string {
	position := lex.position

	for is_digit(lex.ch) {
		lex.read_char()
	}

	return lex.input[position:lex.position]
}

func (lex *Lexer) read_identifier() string {
	position := lex.position

	// Read characters of identifier
	for is_letter(lex.ch) {
		lex.read_char()
	}

	// Return slice containing identifier string
	return lex.input[position:lex.position]
}

// Advance lexer character if whitespace detected
func (lex *Lexer) skip_whitespace() {
	for lex.ch == ' ' || lex.ch == '\t' || lex.ch == '\n' || lex.ch == '\r' {
		lex.read_char()
	}
}

// Exports

func New(input string) *Lexer {
	lex := &Lexer{input: input}
	lex.read_char()

	return lex
}

func (lex *Lexer) NextToken() token.Token {
	var tok token.Token

	lex.skip_whitespace()

	switch lex.ch {
	case 0:
		tok = token.Token{Type: token.EOF, Literal: ""}
	case '=':
		// if another '=' found, advance one char and assign '=='
		if lex.peek_char() == '=' {
			lex.read_char()
			tok = token.Token{Type: token.EQ, Literal: "=="}
		} else {
			tok = new_token(token.ASSIGN, lex.ch)
		}
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
	case '-':
		tok = new_token(token.MINUS, lex.ch)
	case '!':
		if lex.peek_char() == '=' {
			lex.read_char()
			tok = token.Token{Type: token.NE, Literal: "!="}
		} else {
			tok = new_token(token.BANG, lex.ch)
		}
	case '/':
		tok = new_token(token.SLASH, lex.ch)
	case '*':
		tok = new_token(token.ASTERISK, lex.ch)
	case '<':
		tok = new_token(token.LT, lex.ch)
	case '>':
		tok = new_token(token.GT, lex.ch)
	default:
		if is_letter(lex.ch) {
			literal := lex.read_identifier()

			// Early exit prevents read_char from advancing
			return token.Token{Type: token.LookupIdent(literal), Literal: literal}
		} else if is_digit(lex.ch) {
			tok = token.Token{Type: token.INT, Literal: lex.read_number()}
			return tok
		} else {
			tok = new_token(token.ILLEGAL, lex.ch)
		}
	}

	lex.read_char()

	return tok
}
