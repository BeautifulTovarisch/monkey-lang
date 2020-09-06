package parser

import (
	"fmt"

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

func (psr *Parser) parse_let_statement() *ast.LetStatement {
	stmt := &ast.LetStatement{Token: psr.cur_token}

	if !psr.expect_peek(token.IDENT) {
		return nil
	}

	stmt.Name = &ast.Identifier{Token: psr.cur_token, Value: psr.cur_token.Literal}

	if !psr.expect_peek(token.ASSIGN) {
		return nil
	}

	// TODO :: Implement expressions
	for !psr.cur_token_is(token.SEMICOLON) {
		psr.next_token()
	}

	return stmt
}

func (psr *Parser) parse_return_statement() *ast.ReturnStatement {
	stmt := &ast.ReturnStatement{Token: psr.cur_token}

	psr.next_token()

	// TODO :: Implement expressions
	for !psr.cur_token_is(token.SEMICOLON) {
		psr.next_token()
	}

	return stmt
}

func (psr *Parser) parse_identifer() ast.Expression {
	return &ast.Identifier{Token: psr.cur_token, Value: psr.cur_token.Literal}
}

func (psr *Parser) parse_expression(precedence int) ast.Expression {
	prefix := psr.prefix_parse_fns[psr.cur_token.Type]

	if prefix == nil {
		return nil
	}

	left_expr := prefix()

	return left_expr
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
	psr.prefix_parse_fns = make(map[token.TokenType]prefix_parse_fn)
	psr.register_prefix(token.IDENT, psr.parse_identifer)

	return psr
}

func (psr *Parser) Errors() []string {
	return psr.errors
}

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
