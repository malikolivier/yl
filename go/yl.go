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
	for {
		r, _, err := codeBuf.ReadRune()
		if err == io.EOF {
			break
		}
		check(err)
		fmt.Println(r, string(r))
	}
}

func runPrompt() {
	fmt.Println("> ")
}
