package parser

import (
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
	infix_fn  func(ast.Expression, token.Token, []token.Token) (ast.Expression, int)
	prefix_fn func([]token.Token) (ast.Expression, int)
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
// func (psr *Parser) cur_prencendence() int {
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

func parse_infix_expression(left ast.Expression, tok token.Token, tokens []token.Token) (ast.Expression, int) {

	tokens_consumed := 1
	precedence := get_precedence(tok)

	// Expression token
	next, rest := next_token(tokens)
	rhs, consumed := parse_expression(next, rest, precedence)

	return &ast.InfixExpression{
		Left:     left,
		Right:    rhs,
		Token:    tok,
		Operator: tok.Literal,
	}, tokens_consumed + consumed
}

// Close around token -> fn map
func get_infix_fn() func(tok token.TokenType) infix_fn {
	infix_fns := map[token.TokenType]infix_fn{
		token.EQ:       parse_infix_expression,
		token.LT:       parse_infix_expression,
		token.GT:       parse_infix_expression,
		token.NE:       parse_infix_expression,
		token.PLUS:     parse_infix_expression,
		token.MINUS:    parse_infix_expression,
		token.SLASH:    parse_infix_expression,
		token.ASTERISK: parse_infix_expression,
	}

	return func(tok token.TokenType) infix_fn {
		fn, ok := infix_fns[tok]
		if ok {
			return fn
		}

		return nil
	}
}

/* Parses prefix expressions (such as !5 or -15)
 * Current token either ! or -.
 * function advances token in order to capture expression.
 */
func parse_prefix_expression(tokens []token.Token) (ast.Expression, int) {

	// One each for token and RHS
	tokens_consumed := 2

	first, rest := next_token(tokens)
	next, rest := next_token(rest)

	rhs, consumed := parse_expression(next, rest, PREFIX)

	return &ast.PrefixExpression{
		Token:    first,
		Right:    rhs,
		Operator: first.Literal,
	}, tokens_consumed + consumed
}

func get_prefix_fn() func(tok token.TokenType) prefix_fn {
	prefix_fns := map[token.TokenType]prefix_fn{
		// token.IF:       parse_if_expression,
		// token.INT:      parse_integer_literal,
		token.BANG: parse_prefix_expression,
		// token.TRUE:     parse_boolean,
		// token.IDENT:    parse_identifier,
		token.MINUS: parse_prefix_expression,
		// token.FALSE:    parse_boolean,
		// token.LPAREN:   parse_grouped_expression,
		// token.FUNCTION: parse_function_literal,
	}

	return func(tok token.TokenType) prefix_fn {
		fn, ok := prefix_fns[tok]
		if ok {
			return fn
		}

		return nil
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

// func (psr *Parser) parse_grouped_expression() ast.Expression {
// 	psr.next_token()

// 	expr := psr.parse_expression(LOWEST)

// 	if !psr.expect_peek(token.RPAREN) {
// 		return nil
// 	}

// 	return expr
// }

func parse_expression(tok token.Token, tokens []token.Token, precedence int) (ast.Expression, int) {
	tokens_consumed := 2

	// Initialize maps, returning lookup function
	infix_map := get_infix_fn()
	prefix_map := get_prefix_fn()

	prefix := prefix_map(tok.Type)

	if prefix == nil {
		// TODO :: Write logger/error handler to provide feedback to user
		return nil, -1
	}

	left_expr, consumed := prefix(tokens)

	first, rest := next_token(tokens[consumed:])

	for !token_is(first, token.SEMICOLON) &&
		precedence < get_precedence(rest[0]) {

		infix := infix_map(first.Type)

		if infix == nil {
			return left_expr, tokens_consumed + consumed
		}

		tokens_consumed += 1
		next, rest := next_token(rest)

		left_expr, consumed = infix(left_expr, next, rest)

		tokens_consumed += consumed
	}

	return left_expr, tokens_consumed + consumed
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

func parse_statement(tokens []token.Token) (ast.Statement, []token.Token) {
	first, rest := next_token(tokens)

	var stmt ast.Statement
	var consumed int

	switch first.Type {
	case token.EOF:
		return nil, nil
	case token.LET:
		stmt, consumed = parse_let_statement(first, rest)
	case token.RETURN:
		stmt, consumed = parse_return_statement(first, rest)
	default:
		stmt, consumed = parse_expression_statement(first, rest)
	}

	return stmt, tokens[consumed:]
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

	if len(rest) == 0 {
		return []ast.Statement{}
	}

	return append([]ast.Statement{stmt}, ParseProgram(rest)...)
}
