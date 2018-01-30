package main

import (
	"bufio"
	"flag"
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
	codePtr := flag.String("e", "", "inline code")
	flag.Parse()
	tail := flag.Args()
	if *codePtr != "" {
		reader := strings.NewReader(*codePtr)
		run(reader)
	} else if len(tail) != 0 {
		file, err := os.Open(tail[0])
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
