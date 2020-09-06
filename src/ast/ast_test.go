package ast

import (
	"testing"

	"monkey/token"
)

// Roll ast by hand for now
func TestString(t *testing.T) {
	program := &Program{
		Statements: []Statement{
			&LetStatement{
				Token: token.Token{Type: token.LET, Literal: "let"},
				Name: &Identifier{
					Token: token.Token{Type: token.IDENT, Literal: "my_var"},
					Value: "my_var",
				},
				Value: &Identifier{
					Token: token.Token{Type: token.IDENT, Literal: "another_var"},
					Value: "another_var",
				},
			},
		},
	}

	if program.String() != "let my_var = another_var;" {
		t.Errorf("Incorrect string representation. Got %q", program.String())
	}

}
