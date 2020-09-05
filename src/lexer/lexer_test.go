package lexer

import (
	"testing"

	"monkey/token"
)

func TestNextToken(t *testing.T) {
	input := `=+(){},;`

	expected := []struct {
		expectedType    token.TokenType
		expectedLiteral string
	}{
		{token.ASSIGN, "="},
		{token.PLUS, "+"},
		{token.LPAREN, "("},
		{token.RPAREN, ")"},
		{token.LBRACE, "{"},
		{token.RBRACE, "}"},
		{token.COMMA, ","},
		{token.SEMICOLON, ";"},
	}

	lex := New(input)

	for index, tok := range expected {
		token := lex.NextToken()

		if token.Type != tok.expectedType {
			t.Fatalf("expected[%d] - Wrong token. Expected: %q, got %q",
				index, tok.expectedType, token.Type)
		}

		if token.Literal != tok.expectedLiteral {
			t.Fatalf("expected[%d] - Wrong literal. Expected: %q, got %q",
				index, tok.expectedLiteral, token.Literal)
		}
	}
}
