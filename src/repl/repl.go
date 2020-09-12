package repl

import (
	"bufio"
	"fmt"
	"io"

	"monkey/lexer"
	"monkey/token"
)

const PROMPT = ">>"

func Start(in io.Reader, out io.Writer) {
	scanner := bufio.NewScanner(in)

	for {
		fmt.Printf("%s ", PROMPT)

		input := scanner.Scan()

		if !input {
			return
		}

		line := scanner.Text()

		tokens := lexer.Tokenize(line)

		for tok := range tokens {
			fmt.Printf("%+v\n", tok)
		}
	}
}
