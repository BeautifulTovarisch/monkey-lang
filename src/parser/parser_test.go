package parser

import (
	"testing"

	"monkey/ast"
	"monkey/lexer"
)

func test_let_statement(t *testing.T, stmt ast.Statement, name string) bool {
	if stmt.TokenLiteral() != "let" {
		t.Errorf("Incorrect Token. Expected 'let'. Got: %q.", stmt.TokenLiteral())
		return false
	}

	let_stmt, ok := stmt.(*ast.LetStatement)

	if !ok {
		t.Errorf("Invalid type for 'stmt'. Expected '*ast.LetStatement'. Got: %T", stmt)
		return false
	}

	if let_stmt.Name.Value != name {
		t.Errorf("Incorrect value for '.Name.Value'. Expected '%s'. Got %q", name, let_stmt.Name.Value)
		return false
	}

	if let_stmt.Name.TokenLiteral() != name {
		t.Errorf("Incorrect value for TokenLiteral. Expected %q. Got %q", name, let_stmt.Name)
		return false
	}

	return true
}

func check_parser_errors(t *testing.T, psr *Parser) {
	errors := psr.Errors()

	if len(errors) == 0 {
		return
	}

	t.Errorf("Parser found %d errors:", len(errors))

	for i, msg := range errors {
		t.Errorf("%d:\t%q\n", i, msg)
	}

	t.FailNow()
}

func TestLetStatements(t *testing.T) {
	input := `
	let x = 5;
	let y = 10;
	let foobar = 838383;
	`

	lex := lexer.New(input)
	psr := New(lex)

	program := psr.ParseProgram()

	check_parser_errors(t, psr)

	if program == nil {
		t.Fatalf("ParseProgram() returned nil.")
	}

	if len(program.Statements) != 3 {
		t.Fatalf("len(program.Statements) incorrect. Expected 3. Got: %d",
			len(program.Statements))
	}

	identifiers := []struct {
		expected_identfier string
	}{
		{"x"},
		{"y"},
		{"foobar"},
	}

	for i, ident := range identifiers {
		if !test_let_statement(t, program.Statements[i], ident.expected_identfier) {
			return
		}
	}
}

func TestReturnStatements(t *testing.T) {
	input := `
return 5;
return 10;
return 993222;
`

	lex := lexer.New(input)
	psr := New(lex)

	program := psr.ParseProgram()

	check_parser_errors(t, psr)

	if len(program.Statements) != 3 {
		t.Fatalf("len(program.Statements) incorrect. Expected: 3. Got: %d",
			len(program.Statements))
	}

	for _, stmt := range program.Statements {
		ret_stmt, ok := stmt.(*ast.ReturnStatement)

		if !ok {
			t.Errorf("Unexpected type: %T", stmt)
		}

		if ret_stmt.TokenLiteral() != "return" {
			t.Errorf("Unexpected Token. Expected: 'return'. Got: %q.", ret_stmt.TokenLiteral())
		}
	}
}

func TestIdentifierExpression(t *testing.T) {
	input := "foobar;"

	lex := lexer.New(input)
	psr := New(lex)

	program := psr.ParseProgram()

	check_parser_errors(t, psr)

	if len(program.Statements) != 1 {
		t.Fatalf("Wrong number of statements. Got: %d", len(program.Statements))
	}

	stmt, ok := program.Statements[0].(*ast.ExpressionStatement)
	if !ok {
		t.Fatalf("Statement Unexpected type: %T", program.Statements[0])
	}

	ident, ok := stmt.Expression.(*ast.Identifier)
	if !ok {
		t.Fatalf("Expression Unexpected type: %T", stmt.Expression)
	}

	if ident.Value != "foobar" {
		t.Errorf("Incorrect identifier. Expected: 'foobar'. Got: '%s'.", ident.Value)
	}

	if ident.TokenLiteral() != "foobar" {
		t.Errorf("Incorrect token. Expected 'foobar'. Got: %s", ident.TokenLiteral())
	}
}

func TestIntegerLiteralExpressions(t *testing.T) {
	input := "5;"

	lex := lexer.New(input)
	psr := New(lex)

	program := psr.ParseProgram()

	check_parser_errors(t, psr)

	if len(program.Statements) != 1 {
		t.Fatalf("Wrong number of statements. Got: %d", len(program.Statements))
	}

	stmt, ok := program.Statements[0].(*ast.ExpressionStatement)
	if !ok {
		t.Fatalf("Unexpected type: %T for statement.", program.Statements[0])
	}

	literal, ok := stmt.Expression.(*ast.IntegerLiteral)
	if !ok {
		t.Fatalf("Unexpected type: %T for expression", stmt.Expression)
	}

	if literal.Value != 5 {
		t.Errorf("Incorrect Value: %d. Expected 5", literal.Value)
	}

	if literal.TokenLiteral() != "5" {
		t.Errorf("Incorrect TokenLiteral %s. Expected 5", literal.TokenLiteral())
	}

}
