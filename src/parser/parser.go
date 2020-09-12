package parser

import (
	// "fmt"
	// "strconv"

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
	infix_fn  func(ast.Expression) ast.Expression
	prefix_fn func() ast.Expression
)

// type Parser struct {
// 	errors []string

// 	lex        *lexer.Lexer
// 	cur_token  token.Token
// 	peek_token token.Token

// 	infix_parse_fns  map[token.TokenType]infix_parse_fn
// 	prefix_parse_fns map[token.TokenType]prefix_parse_fn
// }

// Append message to errors if next token is not the expected type
// func (psr *Parser) peek_error(tok token.TokenType) {
// 	msg := fmt.Sprintf("Unexpected token. Expected '%s'. Got '%s'.", tok, psr.peek_token.Type)

// 	psr.errors = append(psr.errors, msg)
// }

// Determine precedence of next token. Return lowest if not found
// func (psr *Parser) peek_precedence() int {
// 	if peek, ok := precedences[psr.peek_token.Type]; ok {
// 		return peek
// 	}

// 	return LOWEST
// }

// Retreive precedence of current token
// func (psr *Parser) cur_precendence() int {
// 	if cur, ok := precedences[psr.cur_token.Type]; ok {
// 		return cur
// 	}

// 	return LOWEST
// }

func token_is(tok token.Token, t_type token.TokenType) bool {
	return tok.Type == t_type
}

func next_token(tokens []token.Token) (token.Token, []token.Token) {
	return tokens[0], tokens[1:]
}

// Count number of tokens until terminator (usually semicolon)
// Needed in order to know how many tokens to advance in token list
func count_until(tokens []token.Token, t_type token.TokenType) int {
	if len(tokens) == 0 || token_is(tokens[0], t_type) {
		return 1
	}

	return 1 + count_until(tokens[1:], t_type)
}

// Close around token -> fn map
func get_infix_fn() func(tok token.TokenType) infix_fn {
	infix_fns := map[token.TokenType]infix_fn{}

	return func(tok token.TokenType) infix_fn {
		fn, ok := infix_fns[tok]
		if ok {
			return fn
		}

		return nil
	}
}

func get_prefix_fn() func(tok token.TokenType) prefix_fn {
	prefix_fns := map[token.TokenType]prefix_fn{}

	return func(tok token.TokenType) prefix_fn {
		fn, ok := prefix_fns[tok]
		if ok {
			return fn
		}

		return nil
	}
}

// Determine if provided precedence is less than next
func lower_precedence(precedence int, tok token.Token) bool {
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
		return precedence < prec
	}

	return false
}

// func (psr *Parser) cur_token_is(tok token.TokenType) bool {
// 	return psr.cur_token.Type == tok
// }

// func (psr *Parser) peek_token_is(tok token.TokenType) bool {
// 	return psr.peek_token.Type == tok
// }

// func (psr *Parser) expect_peek(tok token.TokenType) bool {
// 	if psr.peek_token_is(tok) {
// 		psr.next_token()
// 		return true
// 	}

// 	psr.peek_error(tok)
// 	return false
// }

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
func parse_let_statement(let token.Token, tokens []token.Token) (*ast.LetStatement, int) {
	// Always at least one token processed
	tokens_consumed := 1

	ident, rest := next_token(tokens)
	if !token_is(ident, token.IDENT) {
		return nil, 0
	}

	first, rest := next_token(rest)
	if !token_is(first, token.ASSIGN) {
		return nil, 0
	}

	// 1 each for identifier and assignment
	tokens_consumed += 2

	// Count tokens until semicolon
	tokens_consumed += count_until(rest, token.SEMICOLON)

	return &ast.LetStatement{
		Token: let,
		Name:  &ast.Identifier{Token: ident, Value: ident.Literal},
	}, tokens_consumed
}

func parse_return_statement(tok token.Token, tokens []token.Token) (*ast.ReturnStatement, int) {
	tokens_consumed := 1

	_, rest := next_token(tokens)

	tokens_consumed += 1

	// Read tokens until semicolon
	tokens_consumed += count_until(rest, token.SEMICOLON)

	return &ast.ReturnStatement{Token: tok}, tokens_consumed
}

// func (psr *Parser) parse_boolean() ast.Expression {
// 	return &ast.Boolean{Token: psr.cur_token, Value: psr.cur_token_is(token.TRUE)}
// }

func parse_identifier(tok token.Token) ast.Expression {
	return &ast.Identifier{Token: tok, Value: tok.Literal}
}

// func (psr *Parser) parse_integer_literal() ast.Expression {
// 	lit := &ast.IntegerLiteral{Token: psr.cur_token}

// 	value, err := strconv.ParseInt(psr.cur_token.Literal, 0, 64)
// 	if err != nil {
// 		psr.errors = append(psr.errors, fmt.Sprintf("Unable to parse %q.", psr.cur_token.Literal))

// 		return nil
// 	}

// 	lit.Value = value

// 	return lit
// }

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

// func (psr *Parser) parse_infix_expression(left ast.Expression) ast.Expression {
// 	expression := &ast.InfixExpression{
// 		Left:     left,
// 		Token:    psr.cur_token,
// 		Operator: psr.cur_token.Literal,
// 	}

// 	precedence := psr.cur_precendence()
// 	psr.next_token()
// 	expression.Right = psr.parse_expression(precedence)

// 	return expression
// }

/* Parses prefix expressions (such as !5 or -15)
 * Current token either ! or -.
 * function advances token in order to capture expression.
 */
// func (psr *Parser) parse_prefix_expression() ast.Expression {
// 	expression := &ast.PrefixExpression{
// 		Token:    psr.cur_token,
// 		Operator: psr.cur_token.Literal,
// 	}

// 	psr.next_token()

// 	expression.Right = psr.parse_expression(PREFIX)

// 	return expression
// }

// func (psr *Parser) parse_grouped_expression() ast.Expression {
// 	psr.next_token()

// 	expr := psr.parse_expression(LOWEST)

// 	if !psr.expect_peek(token.RPAREN) {
// 		return nil
// 	}

// 	return expr
// }

func parse_expression(tok token.Token, tokens []token.Token, precedence int) (ast.Expression, int) {
	tokens_consumed := 1

	// Initialize maps, returning lookup function
	infix_map := get_infix_fn()
	prefix_map := get_prefix_fn()

	prefix := prefix_map(tok.Type)

	if prefix == nil {
		// TODO :: Write logger/error handler to provide feedback to user
		fmt.Printf("No prefix fn registered for token: %v\n", tok)
		return nil, -1
	}

	left_expr := prefix()

	tokens_consumed += 1
	tok, tokens = next_token(tokens)

	// TODO :: Clean this up (recursively?)
	for !token_is(tok, token.SEMICOLON) &&
		lower_precedence(precedence, tokens[0]) {

		infix := infix_map(tok.Type)

		if infix == nil {
			return left_expr, tokens_consumed
		}

		tokens_consumed += 1
		tok, tokens = next_token(tokens)

		left_expr = infix(left_expr)

	}

	return left_expr, tokens_consumed
}

func parse_expression_statement(tok token.Token, tokens []token.Token) (*ast.ExpressionStatement, int) {
	tokens_consumed := 1

	tokens_consumed += count_until(tokens, token.SEMICOLON)

	exp, consumed := parse_expression(tok, tokens, LOWEST)

	return &ast.ExpressionStatement{
		Token:      tok,
		Expression: exp,
	}, tokens_consumed + consumed
}

func parse_statement(tokens []token.Token) (ast.Statement, int) {
	first, rest := next_token(tokens)

	switch first.Type {
	case token.LET:
		return parse_let_statement(first, rest)
	case token.RETURN:
		return parse_return_statement(first, rest)
		// default:
		// 	return parse_expression_statement(first, rest)
	}

	return nil, 0
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

// 	psr.register_prefix(token.IDENT, psr.parse_identifer)
// 	psr.register_prefix(token.INT, psr.parse_integer_literal)
// 	psr.register_prefix(token.TRUE, psr.parse_boolean)
// 	psr.register_prefix(token.FALSE, psr.parse_boolean)
// 	psr.register_prefix(token.BANG, psr.parse_prefix_expression)
// 	psr.register_prefix(token.MINUS, psr.parse_prefix_expression)
// 	psr.register_prefix(token.LPAREN, psr.parse_grouped_expression)
// 	psr.register_prefix(token.IF, psr.parse_if_expression)
// 	psr.register_prefix(token.FUNCTION, psr.parse_function_literal)

// 	psr.register_infix(token.PLUS, psr.parse_infix_expression)
// 	psr.register_infix(token.MINUS, psr.parse_infix_expression)
// 	psr.register_infix(token.SLASH, psr.parse_infix_expression)
// 	psr.register_infix(token.ASTERISK, psr.parse_infix_expression)
// 	psr.register_infix(token.EQ, psr.parse_infix_expression)
// 	psr.register_infix(token.NE, psr.parse_infix_expression)
// 	psr.register_infix(token.LT, psr.parse_infix_expression)
// 	psr.register_infix(token.GT, psr.parse_infix_expression)
// 	psr.register_infix(token.LPAREN, psr.parse_call_expression)

// 	return psr
// }

// func (psr *Parser) Errors() []string {
// 	return psr.errors
// }

/* Main parser functionality.
 * Kicks off various focused parsing functions.
 */
func ParseProgram(tokens []token.Token) []ast.Statement {
	var recurse func(tokens []token.Token) []ast.Statement

	recurse = func(tokens []token.Token) []ast.Statement {
		if len(tokens) == 0 {
			return []ast.Statement{}
		}

		stmt, tokens_consumed := parse_statement(tokens)

		if tokens_consumed == 0 {
			return nil
		}

		statements := recurse(tokens[tokens_consumed:])

		return append([]ast.Statement{stmt}, statements...)
	}

	return recurse(tokens)
}
