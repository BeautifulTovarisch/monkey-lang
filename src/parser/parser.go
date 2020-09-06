package parser

import (
	"monkey/ast"
	"monkey/lexer"
	"monkey/token"
)

type Parser struct {
	cur_token  token.Token
	peek_token token.Token

	lex *lexer.Lexer
}

func (psr *Parser) next_token() {
	psr.cur_token = psr.peek_token
	psr.peek_token = psr.lex.NextToken()
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

func (psr *Parser) parse_statement() *ast.LetStatement {
	switch psr.cur_token.Type {
	case token.LET:
		return psr.parse_let_statement()
	default:
		return nil
	}
}

// Exported

func New(lex *lexer.Lexer) *Parser {
	psr := &Parser{lex: lex}

	psr.next_token()
	psr.next_token()

	return psr
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
