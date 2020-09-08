package lexer

import (
	"monkey/token"
)

func is_digit(ch byte) bool {
	return '0' <= ch && ch <= '9'
}

func new_token(token_type token.TokenType, ch byte) token.Token {
	return token.Token{Type: token_type, Literal: string(ch)}
}

// Test if ascii letter
// TODO :: Allow unicode in identifiers
func is_letter(ch byte) bool {
	switch true {
	case ch == '_':
		fallthrough
	case 'a' <= ch && ch <= 'z':
		fallthrough
	case 'A' <= ch && ch <= 'Z':
		return true
	}

	return false
}

func read_char(pos int, input string) byte {
	if pos >= len(input) {
		return 0
	}

	return input[pos]
}

/* TODO :: Consider combining the following two functions
 * Accept a termination function, proceed as normal
 * read_until(func(), count int, input string) string {}
 */
func read_number(count int, input string) (int, string) {
	// Recurse until not a digit, 1 is base case in order to handle single digits
	if !is_digit(input[0]) {
		return 0, ""
	}

	advance, next := read_number(count+1, input[1:])

	return advance + 1, string(input[0]) + next
}

func read_identifier(count int, input string) (int, string) {
	// Stop when no longer reading letters
	// Base case of count avoids double counting whitespace
	if len(input) == 0 || !is_letter(input[0]) {
		return 0, ""
	}

	advance, next := read_identifier(count+1, input[1:])

	// Return slice containing identifier string
	return advance + 1, string(input[0]) + next
}

// Returns first non whitespace character after initial index
func skip_whitespace(pos int, input string) (int, byte) {
	whitespace := map[byte]bool{
		' ':  true,
		'\n': true,
		'\t': true,
		'\r': true,
	}

	_, ok := whitespace[input[0]]
	if ok {
		return skip_whitespace(pos+1, input[1:])
	}

	return pos, input[0]
}

/* Scans various inputs/section of a program string.
 * Returns token.Token for parsing at a later step.
 * Additionally returns number of tokens advanced.
 */
func next_token(input string) (token.Token, int) {
	read_chars := 1

	var tok token.Token

	// Get position and value of first non whitespace character
	// Position here also equal to number of whitespace chars skipped
	ws, ch := skip_whitespace(0, input)

	switch ch {
	case '=':
		// if another '=' found, advance one char and assign '=='
		if read_char(ws+1, input) == '=' {
			tok = token.Token{Type: token.EQ, Literal: "=="}
			read_chars += 1
		} else {
			tok = new_token(token.ASSIGN, ch)
		}
	case ';':
		tok = new_token(token.SEMICOLON, ch)
	case '(':
		tok = new_token(token.LPAREN, ch)
	case ')':
		tok = new_token(token.RPAREN, ch)
	case ',':
		tok = new_token(token.COMMA, ch)
	case '+':
		tok = new_token(token.PLUS, ch)
	case '{':
		tok = new_token(token.LBRACE, ch)
	case '}':
		tok = new_token(token.RBRACE, ch)
	case '-':
		tok = new_token(token.MINUS, ch)
	case '!':
		if read_char(ws+1, input) == '=' {
			tok = token.Token{Type: token.NE, Literal: "!="}
			read_chars += 1
		} else {
			tok = new_token(token.BANG, ch)
		}
	case '/':
		tok = new_token(token.SLASH, ch)
	case '*':
		tok = new_token(token.ASTERISK, ch)
	case '<':
		tok = new_token(token.LT, ch)
	case '>':
		tok = new_token(token.GT, ch)
	default:
		if is_letter(ch) {
			advance_by, literal := read_identifier(0, input[ws:])
			tok = token.Token{Type: token.LookupIdent(literal), Literal: literal}

			read_chars = advance_by

		} else if is_digit(ch) {
			advance_by, num := read_number(0, input[ws:])
			tok = token.Token{Type: token.INT, Literal: num}

			read_chars = advance_by
		} else {
			tok = new_token(token.ILLEGAL, ch)
		}
	}

	// Defaults to one character advancement
	return tok, ws + read_chars
}

// Exports

func Tokenize(input string) []token.Token {
	var recurse func(input string) []token.Token

	// Walk the input, building up array of tokens
	recurse = func(input string) []token.Token {

		if len(input) == 0 {
			return []token.Token{{Type: token.EOF, Literal: ""}}
		}

		// Append token to tokens, 'advance' by number of chars
		// processed in lexing the token
		// e.g 'let' -> 3 tokens
		tok, advance := next_token(input)

		token_list := recurse(input[advance:])

		return append([]token.Token{tok}, token_list...)
	}

	return recurse(input)
}
