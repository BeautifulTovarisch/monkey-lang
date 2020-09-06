package parser

import (
	"fmt"

	"monkey/ast"
	"monkey/lexer"
	"monkey/token"
)

type Parser struct {
	errors []string

	lex        *lexer.Lexer
	cur_token  token.Token
	peek_token token.Token
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

func (psr *Parser) parse_statement() ast.Statement {
	switch psr.cur_token.Type {
	case token.LET:
		return psr.parse_let_statement()
	case token.RETURN:
		return psr.parse_return_statement()
	default:
		return nil
	}
}

// Exported

func New(lex *lexer.Lexer) *Parser {
	psr := &Parser{lex: lex, errors: []string{}}

	psr.next_token()
	psr.next_token()

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
