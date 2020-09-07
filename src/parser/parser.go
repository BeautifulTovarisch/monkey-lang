package parser

import (
	"fmt"
	"strconv"

	"monkey/ast"
	"monkey/lexer"
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

var precedences = map[token.TokenType]int{
	token.EQ:       EQUALS,
	token.NE:       EQUALS,
	token.GT:       LESSGREATER,
	token.LT:       LESSGREATER,
	token.PLUS:     SUM,
	token.MINUS:    SUM,
	token.SLASH:    PRODUCT,
	token.ASTERISK: PRODUCT,
}

type (
	infix_parse_fn  func(ast.Expression) ast.Expression
	prefix_parse_fn func() ast.Expression
)

type Parser struct {
	errors []string

	lex        *lexer.Lexer
	cur_token  token.Token
	peek_token token.Token

	infix_parse_fns  map[token.TokenType]infix_parse_fn
	prefix_parse_fns map[token.TokenType]prefix_parse_fn
}

func (psr *Parser) next_token() {
	psr.cur_token = psr.peek_token
	psr.peek_token = psr.lex.NextToken()
}

// Append message to errors if next token is not the expected type
func (psr *Parser) peek_error(tok token.TokenType) {
	msg := fmt.Sprintf("Unexpected token. Expected '%s'. Got '%s'.", tok, psr.peek_token.Type)

	psr.errors = append(psr.errors, msg)
}

// Determine precedence of next token. Return lowest if not found
func (psr *Parser) peek_precedence() int {
	if peek, ok := precedences[psr.peek_token.Type]; ok {
		return peek
	}

	return LOWEST
}

// Retreive precedence of current token
func (psr *Parser) cur_precendence() int {
	if cur, ok := precedences[psr.cur_token.Type]; ok {
		return cur
	}

	return LOWEST
}

func (psr *Parser) cur_token_is(tok token.TokenType) bool {
	return psr.cur_token.Type == tok
}

func (psr *Parser) peek_token_is(tok token.TokenType) bool {
	return psr.peek_token.Type == tok
}

func (psr *Parser) expect_peek(tok token.TokenType) bool {
	if psr.peek_token_is(tok) {
		psr.next_token()
		return true
	}

	psr.peek_error(tok)
	return false
}

func (psr *Parser) parse_block_statement() *ast.BlockStatement {
	block := &ast.BlockStatement{
		Token:      psr.cur_token,
		Statements: []ast.Statement{},
	}

	psr.next_token()

	// if () {
	// ...
	// }
	// ^
	// Collect statements until reaching '}' or end of file (EOF)
	for !psr.cur_token_is(token.RBRACE) && !psr.cur_token_is(token.EOF) {
		stmt := psr.parse_statement()
		if stmt != nil {
			block.Statements = append(block.Statements, stmt)
		}

		psr.next_token()
	}

	return block
}

func (psr *Parser) parse_if_expression() ast.Expression {
	expression := &ast.IfExpression{Token: psr.cur_token}

	// if (
	//    ^
	if !psr.expect_peek(token.LPAREN) {
		return nil
	}

	psr.next_token()
	expression.Condition = psr.parse_expression(LOWEST)

	// if () {
	//     ^
	if !psr.expect_peek(token.RPAREN) {
		return nil
	}

	// if () {
	//       ^
	if !psr.expect_peek(token.LBRACE) {
		return nil
	}

	expression.Consequence = psr.parse_block_statement()

	if psr.peek_token_is(token.ELSE) {
		psr.next_token()

		// else {
		//      ^
		if !psr.expect_peek(token.LBRACE) {
			return nil
		}

		expression.Alternative = psr.parse_block_statement()
	}

	return expression
}

func (psr *Parser) parse_let_statement() *ast.LetStatement {
	stmt := &ast.LetStatement{Token: psr.cur_token}

	// If next token isn't an identifier
	if !psr.expect_peek(token.IDENT) {
		return nil
	}

	stmt.Name = &ast.Identifier{Token: psr.cur_token, Value: psr.cur_token.Literal}

	if !psr.expect_peek(token.ASSIGN) {
		return nil
	}

	// Read tokens until semicolon
	for !psr.cur_token_is(token.SEMICOLON) {
		psr.next_token()
	}

	return stmt
}

func (psr *Parser) parse_return_statement() *ast.ReturnStatement {
	stmt := &ast.ReturnStatement{Token: psr.cur_token}

	psr.next_token()

	// Read tokens until semicolon
	for !psr.cur_token_is(token.SEMICOLON) {
		psr.next_token()
	}

	return stmt
}

func (psr *Parser) parse_boolean() ast.Expression {
	return &ast.Boolean{Token: psr.cur_token, Value: psr.cur_token_is(token.TRUE)}
}

func (psr *Parser) parse_identifer() ast.Expression {
	return &ast.Identifier{Token: psr.cur_token, Value: psr.cur_token.Literal}
}

func (psr *Parser) no_prefix_parse_fn_error(t token.TokenType) {
	msg := fmt.Sprintf("no prefix parse function for %s found", t)
	psr.errors = append(psr.errors, msg)
}

func (psr *Parser) parse_expression(precedence int) ast.Expression {
	prefix := psr.prefix_parse_fns[psr.cur_token.Type]

	if prefix == nil {
		psr.no_prefix_parse_fn_error(psr.cur_token.Type)
		return nil
	}

	left_expr := prefix()

	// Until ';' is reached
	for !psr.peek_token_is(token.SEMICOLON) && precedence < psr.peek_precedence() {
		infix := psr.infix_parse_fns[psr.peek_token.Type]
		if infix == nil {
			return left_expr
		}

		psr.next_token()

		left_expr = infix(left_expr)
	}

	return left_expr
}

func (psr *Parser) parse_integer_literal() ast.Expression {
	lit := &ast.IntegerLiteral{Token: psr.cur_token}

	value, err := strconv.ParseInt(psr.cur_token.Literal, 0, 64)
	if err != nil {
		psr.errors = append(psr.errors, fmt.Sprintf("Unable to parse %q.", psr.cur_token.Literal))

		return nil
	}

	lit.Value = value

	return lit
}

func (psr *Parser) parse_infix_expression(left ast.Expression) ast.Expression {
	expression := &ast.InfixExpression{
		Left:     left,
		Token:    psr.cur_token,
		Operator: psr.cur_token.Literal,
	}

	precedence := psr.cur_precendence()
	psr.next_token()
	expression.Right = psr.parse_expression(precedence)

	return expression
}

/* Parses prefix expressions (such as !5 or -15)
* Current token either ! or -.
* function advances token in order to capture expression.
 */
func (psr *Parser) parse_prefix_expression() ast.Expression {
	expression := &ast.PrefixExpression{
		Token:    psr.cur_token,
		Operator: psr.cur_token.Literal,
	}

	psr.next_token()

	expression.Right = psr.parse_expression(PREFIX)

	return expression
}

func (psr *Parser) parse_grouped_expression() ast.Expression {
	psr.next_token()

	expr := psr.parse_expression(LOWEST)

	if !psr.expect_peek(token.RPAREN) {
		return nil
	}

	return expr
}

func (psr *Parser) parse_expression_statement() *ast.ExpressionStatement {
	stmt := &ast.ExpressionStatement{
		Token:      psr.cur_token,
		Expression: psr.parse_expression(LOWEST),
	}

	if psr.peek_token_is(token.SEMICOLON) {
		psr.next_token()
	}

	return stmt
}

func (psr *Parser) parse_statement() ast.Statement {
	switch psr.cur_token.Type {
	case token.LET:
		return psr.parse_let_statement()
	case token.RETURN:
		return psr.parse_return_statement()
	default:
		return psr.parse_expression_statement()
	}
}

func (psr *Parser) register_infix(token_type token.TokenType, fn infix_parse_fn) {
	psr.infix_parse_fns[token_type] = fn
}

func (psr *Parser) register_prefix(token_type token.TokenType, fn prefix_parse_fn) {
	psr.prefix_parse_fns[token_type] = fn
}

// Exported

func New(lex *lexer.Lexer) *Parser {
	psr := &Parser{lex: lex, errors: []string{}}

	psr.next_token()
	psr.next_token()

	// Associate parsing functions
	psr.infix_parse_fns = make(map[token.TokenType]infix_parse_fn)
	psr.prefix_parse_fns = make(map[token.TokenType]prefix_parse_fn)

	psr.register_prefix(token.IDENT, psr.parse_identifer)
	psr.register_prefix(token.INT, psr.parse_integer_literal)

	psr.register_prefix(token.TRUE, psr.parse_boolean)
	psr.register_prefix(token.FALSE, psr.parse_boolean)

	psr.register_prefix(token.BANG, psr.parse_prefix_expression)
	psr.register_prefix(token.MINUS, psr.parse_prefix_expression)

	psr.register_prefix(token.LPAREN, psr.parse_grouped_expression)

	psr.register_prefix(token.IF, psr.parse_if_expression)

	psr.register_infix(token.PLUS, psr.parse_infix_expression)
	psr.register_infix(token.MINUS, psr.parse_infix_expression)
	psr.register_infix(token.SLASH, psr.parse_infix_expression)
	psr.register_infix(token.ASTERISK, psr.parse_infix_expression)
	psr.register_infix(token.EQ, psr.parse_infix_expression)
	psr.register_infix(token.NE, psr.parse_infix_expression)
	psr.register_infix(token.LT, psr.parse_infix_expression)
	psr.register_infix(token.GT, psr.parse_infix_expression)

	return psr
}

func (psr *Parser) Errors() []string {
	return psr.errors
}

/* Main parser functionality.
*
 */
func (psr *Parser) ParseProgram() *ast.Program {
	program := &ast.Program{}
	program.Statements = []ast.Statement{}

	for psr.cur_token.Type != token.EOF {
		stmt := psr.parse_statement()

		if stmt != nil {
			program.Statements = append(program.Statements, stmt)
		}
		psr.next_token()
	}

	return program
}
