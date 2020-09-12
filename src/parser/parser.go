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

type (
	infix_fn  func(ast.Expression, token.Token, []token.Token) (ast.Expression, int)
	prefix_fn func([]token.Token) (ast.Expression, int)
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

func parse_integer_literal(intgr token.Token) ast.Expression {
	value, err := strconv.ParseInt(intgr.Literal, 0, 64)
	if err != nil {
		return nil
	}

	return &ast.IntegerLiteral{Token: intgr, Value: value}
}

func parse_prefix(tok token.Token, tokens []token.Token) (ast.Expression, []token.Token) {

	switch tok.Type {
	case token.INT:
		return parse_integer_literal(tok), tokens
	case token.BANG:
		fallthrough
	case token.MINUS:
		return parse_prefix_expression(tok, tokens)
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

// func (psr *Parser) parse_block_statement() *ast.BlockStatement {
// 	block := &ast.BlockStatement{
// 		Token:      psr.cur_token,
// 		Statements: []ast.Statement{},
// 	}

// 	psr.next_token()

// 	// if () {
// 	// ...
// 	// }
// 	// ^
// 	// Collect statements until reaching '}' or end of file (EOF)
// 	for !psr.cur_token_is(token.RBRACE) && !psr.cur_token_is(token.EOF) {
// 		stmt := psr.parse_statement()
// 		if stmt != nil {
// 			block.Statements = append(block.Statements, stmt)
// 		}

// 		psr.next_token()
// 	}

// 	return block
// }

// func (psr *Parser) parse_if_expression() ast.Expression {
// 	expression := &ast.IfExpression{Token: psr.cur_token}

// 	// if (
// 	//    ^
// 	if !psr.expect_peek(token.LPAREN) {
// 		return nil
// 	}

// 	psr.next_token()
// 	expression.Condition = psr.parse_expression(LOWEST)

// 	// if () {
// 	//     ^
// 	if !psr.expect_peek(token.RPAREN) {
// 		return nil
// 	}

// 	// if () {
// 	//       ^
// 	if !psr.expect_peek(token.LBRACE) {
// 		return nil
// 	}

// 	expression.Consequence = psr.parse_block_statement()

// 	if psr.peek_token_is(token.ELSE) {
// 		psr.next_token()

// 		// else {
// 		//      ^
// 		if !psr.expect_peek(token.LBRACE) {
// 			return nil
// 		}

// 		expression.Alternative = psr.parse_block_statement()
// 	}

// 	return expression
// }

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

// func (psr *Parser) parse_boolean() ast.Expression {
// 	return &ast.Boolean{Token: psr.cur_token, Value: psr.cur_token_is(token.TRUE)}
// }

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

// func (psr *Parser) parse_grouped_expression() ast.Expression {
// 	psr.next_token()

// 	expr := psr.parse_expression(LOWEST)

// 	if !psr.expect_peek(token.RPAREN) {
// 		return nil
// 	}

// 	return expr
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

		infix, remaining := parse_infix_expression(left_expr, tok, remaining)

		return infix, remaining
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

// func (psr *Parser) register_infix(token_type token.TokenType, fn infix_parse_fn) {
// 	psr.infix_parse_fns[token_type] = fn
// }

// func (psr *Parser) register_prefix(token_type token.TokenType, fn prefix_parse_fn) {
// 	psr.prefix_parse_fns[token_type] = fn
// }

// Exported

// func New(lex *lexer.Lexer) *Parser {
// 	psr := &Parser{lex: lex, errors: []string{}}

// 	psr.next_token()
// 	psr.next_token()

// 	// Associate parsing functions
// 	psr.infix_parse_fns = make(map[token.TokenType]infix_parse_fn)
// 	psr.prefix_parse_fns = make(map[token.TokenType]prefix_parse_fn)

// 	return psr
// }

// func (psr *Parser) Errors() []string {
// 	return psr.errors
// }

/* Main parser functionality.
 * Kicks off various focused parsing functions.
 */
func ParseProgram(tokens []token.Token) []ast.Statement {

	stmt, rest := parse_statement(tokens)

	fmt.Println(rest)

	if len(rest) == 0 {
		return []ast.Statement{}
	}

	return append([]ast.Statement{stmt}, ParseProgram(rest)...)
}
