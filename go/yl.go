package main

import (
	"bufio"
	"fmt"
	"io"
	"os"
	"strings"
)

func check(e error) {
	if e != nil {
		panic(e)
	}
}

func main() {
	args := os.Args[1:]
	if len(args) >= 2 && args[0] == "-e" {
		reader := strings.NewReader(args[1])
		run(reader)
	} else if len(args) >= 1 {
		file, err := os.Open(args[0])
		check(err)
		reader := bufio.NewReader(file)
		run(reader)
	} else {
		runPrompt()
	}
}

func run(codeBuf io.RuneReader) {
	ast, err := parse(codeBuf)
	check(err)
	scope := createParentScope()
	ret := evaluate(ast, &scope, false)
	os.Exit(varToInt(ret))
}

func runPrompt() {
	scope := createParentScope()
	reader := bufio.NewReader(os.Stdin)
	for {
		fmt.Print("> ")
		code, err := reader.ReadString('\n')
		check(err)
		ast, err := parse(strings.NewReader(code))
		check(err)
		ret := evaluate(ast, &scope, false)
		fmt.Println(varToString(ret))
	}
}
