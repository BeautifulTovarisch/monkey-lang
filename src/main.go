package main

import (
	"fmt"
	"os"
	"os/user"

	"monkey/repl"
)

func main() {
	_, err := user.Current()

	if err != nil {
		panic(err)
	}

	fmt.Println("Monkey Lang v0.0.1")

	repl.Start(os.Stdin, os.Stdout)
}
