package parser

import (
	"fmt"
	"strconv"

	"monkey/ast"
	"monkey/token"
)

// Operator precedence
const (
	_ int = iota
	LOWEST
	EQUALS
	LESSGREATER
	SUM
	PRODUCT
	PREFIX
	CALL
)

func token_is(tok token.Token, t_type token.TokenType) bool {
	return tok.Type == t_type
}

func next_token(tokens []token.Token) (token.Token, []token.Token) {
	if len(tokens) == 0 {
		return token.Token{}, []token.Token{}
	}

	return tokens[0], tokens[1:]
}

// Slice tokens until desired token type is reached
func advance_until(tokens []token.Token, t_type token.TokenType) []token.Token {
	if len(tokens) == 0 || token_is(tokens[0], t_type) {
		return tokens
	}

	return advance_until(tokens[1:], t_type)
}

func parse_integer_literal(intgr token.Token) ast.Expression {
	value, err := strconv.ParseInt(intgr.Literal, 0, 64)
	if err != nil {
		return nil
	}

	return &ast.IntegerLiteral{Token: intgr, Value: value}
}

/* Parses prefix expressions (such as !5 or -15)
 * Current token either ! or -.
 * function advances token in order to capture expression.
 */
func parse_prefix_expression(tok token.Token, tokens []token.Token) (ast.Expression, []token.Token) {

	first, rest := next_token(tokens)

	rhs, remaining := parse_expression(first, rest, PREFIX)

	return &ast.PrefixExpression{
		Token:    tok,
		Right:    rhs,
		Operator: tok.Literal,
	}, remaining
}

func parse_block_statement(tok token.Token, tokens []token.Token) (*ast.BlockStatement, []token.Token) {

	var statements []ast.Statement

	// if () {
	// ...
	// }
	// ^
	// Collect statements until reaching '}' or end of file (EOF)
	for !token_is(tok, token.RBRACE) && !token_is(tok, token.EOF) {
		stmt, remaining := parse_statement(tokens)

		if stmt != nil {
			statements = append(statements, stmt)
		}

		// Advance token in loop condition
		tok, _ = next_token(remaining)
	}

	// advance_until() should yield same slice as advancing in loop
	return &ast.BlockStatement{
		Token:      tok,
		Statements: statements,
	}, advance_until(tokens, token.RBRACE)
}

func parse_grouped_expression(tok token.Token, tokens []token.Token) (ast.Expression, []token.Token) {

	expr, remaining := parse_expression(tok, tokens, LOWEST)

	next, rest := next_token(remaining)

	if !token_is(next, token.RPAREN) {
		return nil, []token.Token{}
	}

	return expr, rest
}

func parse_if_expression(tok token.Token, tokens []token.Token) (ast.Expression, []token.Token) {

	var alternative *ast.BlockStatement

	lparen, rest := next_token(tokens)

	// if (
	//    ^
	if !token_is(lparen, token.LPAREN) {
		return nil, []token.Token{}
	}

	// Conditional expression
	next, rest := next_token(rest)

	condition, remaining := parse_expression(next, rest, LOWEST)

	rparen, rest := next_token(remaining)

	// if () {
	//     ^
	if !token_is(rparen, token.RPAREN) {
		return nil, []token.Token{}
	}

	lbrace, rest := next_token(rest)

	// if () {
	//       ^
	if !token_is(lbrace, token.LBRACE) {
		return nil, []token.Token{}
	}

	// if () {
	// > ...
	// }
	consequence, rest := parse_block_statement(lbrace, rest)

	fmt.Println("consequence", consequence)

	elseblk, rest := next_token(rest)

	if token_is(elseblk, token.ELSE) {
		lbrace, rest := next_token(rest)

		// else {
		//      ^
		if !token_is(lbrace, token.LBRACE) {
			return nil, []token.Token{}
		}

		alternative, rest = parse_block_statement(lbrace, rest)
	}

	return &ast.IfExpression{
		Token:       tok,
		Condition:   condition,
		Alternative: alternative,
		Consequence: consequence,
	}, rest
}

func parse_prefix(tok token.Token, tokens []token.Token) (ast.Expression, []token.Token) {

	switch tok.Type {
	case token.IF:
		return parse_if_expression(tok, tokens)
	case token.INT:
		return parse_integer_literal(tok), tokens
	case token.IDENT:
		return parse_identifier(tok), tokens
	// Boolean
	case token.TRUE:
		fallthrough
	case token.FALSE:
		// tokens[1:] here to advance past semicolon
		return parse_boolean(tok), tokens[1:]
	// Prefix expression
	case token.BANG:
		fallthrough
	case token.MINUS:
		return parse_prefix_expression(tok, tokens)
	case token.LPAREN:
		return parse_grouped_expression(tok, tokens)
	default:
		return nil, tokens
	}
}

// Determine if provided precedence is less than next
func get_precedence(tok token.Token) int {
	precedences := map[token.TokenType]int{
		token.EQ:       EQUALS,
		token.NE:       EQUALS,
		token.GT:       LESSGREATER,
		token.LT:       LESSGREATER,
		token.PLUS:     SUM,
		token.MINUS:    SUM,
		token.SLASH:    PRODUCT,
		token.ASTERISK: PRODUCT,
	}

	if prec, ok := precedences[tok.Type]; ok {
		return prec
	}

	return LOWEST
}

// Parse let statement, returning statement and number of tokens 'consumed'
func parse_let_statement(let token.Token, tokens []token.Token) (*ast.LetStatement, []token.Token) {

	ident, rest := next_token(tokens)
	if !token_is(ident, token.IDENT) {
		return nil, []token.Token{}
	}

	first, rest := next_token(rest)
	if !token_is(first, token.ASSIGN) {
		return nil, []token.Token{}
	}

	// Advance tokens until semicolon (or no token remaining)
	remaining := advance_until(rest, token.SEMICOLON)

	// Return remaining[1:] to advance past semicolon
	return &ast.LetStatement{
		Token: let,
		Name:  &ast.Identifier{Token: ident, Value: ident.Literal},
	}, remaining[1:]
}

func parse_return_statement(ret token.Token, tokens []token.Token) (*ast.ReturnStatement, []token.Token) {
	_, rest := next_token(tokens)

	// Read tokens until semicolon
	remaining := advance_until(rest, token.SEMICOLON)

	// Return remaining[1:] to advance past semicolon
	return &ast.ReturnStatement{Token: ret}, remaining[1:]
}

func parse_boolean(tok token.Token) ast.Expression {
	return &ast.Boolean{Token: tok, Value: tok.Type == token.TRUE}
}

func parse_identifier(tok token.Token) ast.Expression {
	return &ast.Identifier{Token: tok, Value: tok.Literal}
}

// func (psr *Parser) parse_call_arguments() []ast.Expression {
// 	args := []ast.Expression{}

// 	if psr.peek_token_is(token.RPAREN) {
// 		psr.next_token()
// 		return args
// 	}

// 	psr.next_token()
// 	args = append(args, psr.parse_expression(LOWEST))

// 	for psr.peek_token_is(token.COMMA) {
// 		psr.next_token()
// 		psr.next_token()

// 		args = append(args, psr.parse_expression(LOWEST))
// 	}

// 	if !psr.expect_peek(token.RPAREN) {
// 		return nil
// 	}

// 	return args
// }

// func (psr *Parser) parse_call_expression(fn ast.Expression) ast.Expression {
// 	return &ast.CallExpression{
// 		Token:     psr.cur_token,
// 		Function:  fn,
// 		Arguments: psr.parse_call_arguments(),
// 	}
// }

// func (psr *Parser) parse_function_parameters() []*ast.Identifier {
// 	identifiers := []*ast.Identifier{}

// 	// fn (...)
// 	//        ^
// 	// We've read all parameters, return
// 	if psr.peek_token_is(token.RPAREN) {
// 		psr.next_token()
// 		return identifiers
// 	}

// 	psr.next_token()

// 	ident := &ast.Identifier{Token: psr.cur_token, Value: psr.cur_token.Literal}
// 	identifiers = append(identifiers, ident)

// 	// fn (x, ...)
// 	//      ^
// 	// If the next token is a comma, advance twice
// 	// read additional parameter
// 	for psr.peek_token_is(token.COMMA) {
// 		psr.next_token()
// 		psr.next_token()

// 		ident := &ast.Identifier{Token: psr.cur_token, Value: psr.cur_token.Literal}
// 		identifiers = append(identifiers, ident)
// 	}

// 	if !psr.expect_peek(token.RPAREN) {
// 		return nil
// 	}

// 	return identifiers
// }

// func (psr *Parser) parse_function_literal() ast.Expression {
// 	fn := &ast.FunctionLiteral{Token: psr.cur_token}

// 	if !psr.expect_peek(token.LPAREN) {
// 		return nil
// 	}

// 	fn.Parameters = psr.parse_function_parameters()

// 	if !psr.expect_peek(token.LBRACE) {
// 		return nil
// 	}

// 	fn.Body = psr.parse_block_statement()

// 	return fn
// }

func parse_infix_expression(left ast.Expression, tok token.Token, tokens []token.Token) (ast.Expression, []token.Token) {

	// Expression token
	next, rest := next_token(tokens)

	rhs, remaining := parse_expression(next, rest, get_precedence(tok))

	return &ast.InfixExpression{
		Left:     left,
		Right:    rhs,
		Token:    tok,
		Operator: tok.Literal,
	}, remaining
}

func parse_expression(tok token.Token, tokens []token.Token, precedence int) (ast.Expression, []token.Token) {

	// May need to pass in tokens[1:] here...
	left_expr, remaining := parse_prefix(tok, tokens)

	for !token_is(tok, token.SEMICOLON) &&
		precedence < get_precedence(remaining[0]) {

		tok, remaining = next_token(remaining)

		return parse_infix_expression(left_expr, tok, remaining)
	}

	return left_expr, remaining
}

func parse_expression_statement(tok token.Token, tokens []token.Token) (*ast.ExpressionStatement, []token.Token) {

	exp, remaining := parse_expression(tok, tokens, LOWEST)

	return &ast.ExpressionStatement{
		Token:      tok,
		Expression: exp,
	}, remaining
}

func parse_statement(tokens []token.Token) (ast.Statement, []token.Token) {
	first, rest := next_token(tokens)

	switch first.Type {
	case token.EOF:
		return nil, []token.Token{}
	case token.LET:
		return parse_let_statement(first, rest)
	case token.RETURN:
		return parse_return_statement(first, rest)
	default:
		return parse_expression_statement(first, rest)
	}
}

// Exported

/* Main parser functionality.
 * Kicks off various focused parsing functions.
 */
func ParseProgram(tokens []token.Token) []ast.Statement {

	stmt, rest := parse_statement(tokens)

	fmt.Println()

	if len(rest) == 0 {
		return []ast.Statement{}
	}

	return append([]ast.Statement{stmt}, ParseProgram(rest)...)
}
