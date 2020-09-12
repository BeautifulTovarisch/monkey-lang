package parser

import (
	"fmt"
	"testing"

	"monkey/ast"
	"monkey/lexer"
)

func type_error(t *testing.T, expected string, actual interface{}) {
	t.Fatalf("Unexpected type: %T for %s", actual, expected)
}

func statement_error(t *testing.T, expected int, actual int) {
	t.Fatalf("Wrong number of statements. Expected: %d, Got: %d", expected, actual)
}

func test_let_statement(t *testing.T, stmt ast.Statement, name string) bool {
	if stmt.TokenLiteral() != "let" {
		t.Errorf("Incorrect Token. Expected 'let'. Got: %q.", stmt.TokenLiteral())
		return false
	}

	let_stmt, ok := stmt.(*ast.LetStatement)

	if !ok {
		type_error(t, "LetStatement", stmt)
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

func test_integer_literal(t *testing.T, i_lit ast.Expression, value int64) bool {
	intgr, ok := i_lit.(*ast.IntegerLiteral)
	if !ok {
		type_error(t, "IntegerLiteral", i_lit)
		return false
	}

	if intgr.Value != value {
		t.Errorf("Incorrect value %d. Expected %d", intgr.Value, value)
		return false
	}

	if intgr.TokenLiteral() != fmt.Sprintf("%d", value) {
		t.Errorf("Incorrect token literal: %s. Expected %d.", intgr.TokenLiteral(), value)
		return false
	}

	return true
}

// Test Suite

func TestLetStatements(t *testing.T) {
	input := `
	let x = 5;
	let y = 10;
	let foobar = 838383;`

	statements := ParseProgram(lexer.Tokenize(input))

	// check_parser_errors(t, syntax_tree)

	if len(statements) == 0 {
		t.Fatalf("ParseProgram() returned no statements.")
	}

	if len(statements) != 3 {
		statement_error(t, 3, len(statements))
	}

	identifiers := []struct {
		expected_identfier string
	}{
		{"x"},
		{"y"},
		{"foobar"},
	}

	for i, ident := range identifiers {
		if !test_let_statement(t, statements[i], ident.expected_identfier) {
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

	statements := ParseProgram(lexer.Tokenize(input))

	if len(statements) != 3 {
		statement_error(t, 3, len(statements))
	}

	for _, stmt := range statements {
		ret_stmt, ok := stmt.(*ast.ReturnStatement)

		if !ok {
			type_error(t, "ReturnStatement", stmt)
		}

		if ret_stmt.TokenLiteral() != "return" {
			t.Errorf("Unexpected Token. Expected: 'return'. Got: %q.", ret_stmt.TokenLiteral())
		}
	}
}

func TestBooleanExpression(t *testing.T) {

	tokens := lexer.Tokenize("true;")

	statements := ParseProgram(tokens)

	if len(statements) != 1 {
		t.Fatalf("Wrong number of statements. Got: %d", len(statements))
	}

	stmt, ok := statements[0].(*ast.ExpressionStatement)
	if !ok {
		type_error(t, "ExpressionStatement", statements[0])
	}

	ident, ok := stmt.Expression.(*ast.Boolean)
	if !ok {
		type_error(t, "Boolean", stmt.Expression)
	}

	if ident.Value != true {
		t.Errorf("Incorrect value. Expected: 'true'. Got: '%t'.", ident.Value)
	}

	if ident.TokenLiteral() != "true" {
		t.Errorf("Incorrect token. Expected 'true'. Got: %s", ident.TokenLiteral())
	}
}

// func TestIdentifierExpression(t *testing.T) {
// 	input := "foobar;"

// 	lex := lexer.New(input)
// 	psr := New(lex)

// 	program := psr.ParseProgram()

// 	check_parser_errors(t, psr)

// 	if len(statements) != 1 {
// 		t.Fatalf("Wrong number of statements. Got: %d", len(statements))
// 	}

// 	stmt, ok := statements[0].(*ast.ExpressionStatement)
// 	if !ok {
// 		type_error(t, "ExpressionStatement", statements[0])
// 	}

// 	ident, ok := stmt.Expression.(*ast.Identifier)
// 	if !ok {
// 		type_error(t, "Identifier", stmt.Expression)
// 	}

// 	if ident.Value != "foobar" {
// 		t.Errorf("Incorrect identifier. Expected: 'foobar'. Got: '%s'.", ident.Value)
// 	}

// 	if ident.TokenLiteral() != "foobar" {
// 		t.Errorf("Incorrect token. Expected 'foobar'. Got: %s", ident.TokenLiteral())
// 	}
// }

// func TestIntegerLiteralExpressions(t *testing.T) {
// 	input := "5;"

// 	lex := lexer.New(input)
// 	psr := New(lex)

// 	program := psr.ParseProgram()

// 	check_parser_errors(t, psr)

// 	if len(statements) != 1 {
// 		t.Fatalf("Wrong number of statements. Got: %d", len(statements))
// 	}

// 	stmt, ok := statements[0].(*ast.ExpressionStatement)
// 	if !ok {
// 		t.Fatalf("Unexpected type: %T for statement.", statements[0])
// 	}

// 	literal, ok := stmt.Expression.(*ast.IntegerLiteral)
// 	if !ok {
// 		t.Fatalf("Unexpected type: %T for expression", stmt.Expression)
// 	}

// 	if literal.Value != 5 {
// 		t.Errorf("Incorrect Value: %d. Expected 5", literal.Value)
// 	}

// 	if literal.TokenLiteral() != "5" {
// 		t.Errorf("Incorrect TokenLiteral %s. Expected 5", literal.TokenLiteral())
// 	}

// }

// func TestParsingPrefixExpressions(t *testing.T) {
// 	prefix_tests := []struct {
// 		input     string
// 		operator  string
// 		int_value int64
// 	}{
// 		{"!5", "!", 5},
// 		{"-15", "-", 15},
// 	}

// 	for _, test := range prefix_tests {
// 		lex := lexer.New(test.input)
// 		psr := New(lex)

// 		program := psr.ParseProgram()

// 		check_parser_errors(t, psr)

// 		if len(statements) != 1 {
// 			t.Fatalf("Wrong number of statements: %d", len(statements))
// 		}

// 		stmt, ok := statements[0].(*ast.ExpressionStatement)
// 		if !ok {
// 			t.Fatalf("Unexpected type: %T for statement.", statements[0])
// 		}

// 		exp, ok := stmt.Expression.(*ast.PrefixExpression)
// 		if !ok {
// 			t.Fatalf("Unexpected type: %T for expression", stmt.Expression)
// 		}

// 		if exp.Operator != test.operator {
// 			t.Fatalf("Unexpected operator %s. Expected %s.", exp.Operator, test.operator)
// 		}

// 		if !test_integer_literal(t, exp.Right, test.int_value) {
// 			return
// 		}
// 	}
// }

func TestParsingInfixExpressions(t *testing.T) {
	infix_tests := []struct {
		input     string
		left_val  int64
		operator  string
		right_val int64
	}{
		{"5 + 5", 5, "+", 5},
		{"5 - 5", 5, "-", 5},
		{"5 * 5", 5, "*", 5},
		{"5 / 5", 5, "/", 5},
		{"5 > 5", 5, ">", 5},
		{"5 < 5", 5, "<", 5},
		{"5 == 5", 5, "==", 5},
		{"5 != 5", 5, "!=", 5},
	}

	for _, test := range infix_tests {
		tokens := lexer.Tokenize(test.input)
		statements := ParseProgram(tokens)

		if len(statements) != 1 {
			t.Fatalf("Wrong number of statements. Got: %d", len(statements))
		}

		stmt, ok := statements[0].(*ast.ExpressionStatement)
		if !ok {
			type_error(t, "ExpressionStatement", statements[0])
		}

		exp, ok := stmt.Expression.(*ast.InfixExpression)
		if !ok {
			type_error(t, "InfixExpression", stmt.Expression)
		}

		if exp.Operator != test.operator {
			t.Fatalf("Unexpected operator: %s. Expected %s", exp.Operator, test.operator)
		}

		if !test_integer_literal(t, exp.Left, test.left_val) {
			return
		}
	}
}

// func TestOperatorPrecedence(t *testing.T) {
// 	tests := []struct {
// 		input    string
// 		expected string
// 	}{
// 		{"!-a", "(!(-a))"},
// 		{"a + b + c", "((a + b) + c)"},
// 		{"a - b - c", "((a - b) - c)"},
// 		{"a + b * c", "(a + (b * c))"},
// 		{"a * b / c", "((a * b) / c)"},
// 		{"a + b / c", "(a + (b / c))"},
// 		{"3 + 4; -5 * 5", "(3 + 4)((-5) * 5)"},
// 		{"1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"},
// 		{"5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"},
// 		{"5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"},
// 	}

// 	for _, test := range tests {
// 		lex := lexer.New(test.input)
// 		psr := New(lex)

// 		program := psr.ParseProgram()

// 		check_parser_errors(t, psr)

// 		actual := program.String()

// 		if actual != test.expected {
// 			t.Errorf("Unexpected output. Expected %q. Got %q", test.expected, actual)
// 		}
// 	}
// }

// func TestIfStatement(t *testing.T) {
// 	input := `if (x < y) { x }`

// 	lex := lexer.New(input)
// 	psr := New(lex)

// 	program := psr.ParseProgram()

// 	check_parser_errors(t, psr)

// 	if len(statements) != 1 {
// 		t.Fatalf("Wrong number of statements. Got: %d", len(statements))
// 	}

// 	stmt, ok := statements[0].(*ast.ExpressionStatement)
// 	if !ok {
// 		t.Fatalf("Unexpected type: %T for ExpressionStatement", statements[0])
// 	}

// 	exp, ok := stmt.Expression.(*ast.IfExpression)
// 	if !ok {
// 		t.Fatalf("Unexpected type: %T for IfExpression", stmt.Expression)
// 	}

// 	if len(exp.Consequence.Statements) != 1 {
// 		t.Errorf("Wrong number of Statements: %d", len(exp.Consequence.Statements))
// 	}

// 	consequence, ok := exp.Consequence.Statements[0].(*ast.ExpressionStatement)
// 	if !ok {
// 		t.Errorf("Unexpected type: %T for ExpressionStatement", exp.Consequence.Statements[0])
// 	}

// 	if consequence == nil {
// 		return
// 	}
// }

// func TestFunctionLiteral(t *testing.T) {
// 	input := `fn(x, y) { x + y; }`

// 	lex := lexer.New(input)
// 	psr := New(lex)

// 	program := psr.ParseProgram()

// 	check_parser_errors(t, psr)

// 	if len(statements) != 1 {
// 		t.Fatalf("Wrong number of statements. Got: %d", len(statements))
// 	}

// 	stmt, ok := statements[0].(*ast.ExpressionStatement)
// 	if !ok {
// 		type_error(t, "ExpressionStatement", statements[0])
// 	}

// 	exp, ok := stmt.Expression.(*ast.FunctionLiteral)
// 	if !ok {
// 		type_error(t, "FunctionLiteral", stmt.Expression)
// 	}

// 	if len(exp.Body.Statements) != 1 {
// 		t.Errorf("Wrong number of Statements: %d", len(exp.Body.Statements))
// 	}
// }
