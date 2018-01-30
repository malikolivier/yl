package main

import (
	"bytes"
	"io"
)

const (
	AstNode = iota
	AstList
)

type Ast struct {
	kind int
	node string
	list []Ast
}

const (
	TokenOpen = iota
	TokenClose
	TokenSymbol
)

type Token struct {
	kind int
	val  string
}

type TokenReader struct {
	buf             io.RuneReader
	anyBufferedRune bool
	bufferedRune    rune
}

func peekRune(reader *TokenReader) (rune, error) {
	if reader.anyBufferedRune {
		return reader.bufferedRune, nil
	} else {
		r, _, err := reader.buf.ReadRune()
		if err == nil {
			reader.anyBufferedRune = true
			reader.bufferedRune = r
		}
		return r, err
	}
}

func nextRune(reader *TokenReader) (rune, error) {
	if reader.anyBufferedRune {
		reader.anyBufferedRune = false
		return reader.bufferedRune, nil
	} else {
		r, _, err := reader.buf.ReadRune()
		return r, err
	}
}

func readWhile(reader *TokenReader, predicate func(rune) bool) (string, error) {
	var buffer bytes.Buffer
	r, err := peekRune(reader)
	for err == nil && predicate(r) {
		nextRune(reader)
		buffer.WriteRune(r)
		r, err = peekRune(reader)
	}
	if err == io.EOF || err == nil {
		return buffer.String(), nil
	} else {
		return "", err
	}
}

func skipComment(reader *TokenReader) {
	readWhile(reader, func(r rune) bool {
		return r != '\n'
	})
	nextRune(reader)
}

func readEscaped(reader *TokenReader) (string, error) {
	var buffer bytes.Buffer
	escaped := false
	nextRune(reader)
	r, err := nextRune(reader)
	for err == nil {
		if escaped {
			buffer.WriteRune(r)
			escaped = false
		} else if r == '\\' {
			escaped = true
		} else if r == '"' {
			break
		} else {
			buffer.WriteRune(r)
		}
		r, err = nextRune(reader)
	}
	if err == io.EOF || err == nil {
		return buffer.String(), nil
	} else {
		return "", err
	}
}

func readString(reader *TokenReader) (Token, error) {
	str, err := readEscaped(reader)
	return Token{TokenSymbol, str}, err
}

func readSymbol(reader *TokenReader) (Token, error) {
	str, err := readWhile(reader, func(r rune) bool {
		return r != '\n' && r != ' ' && r != '\t' && r != '(' && r != ')'
	})
	return Token{TokenSymbol, str}, err
}

func nextToken(reader *TokenReader) (Token, error) {
	r, err := peekRune(reader)
	if err != nil {
		return Token{-1, ""}, err
	}
	switch r {
	case ' ', '\t', '\n':
		nextRune(reader)
		return nextToken(reader)
	case ';':
		skipComment(reader)
		return nextToken(reader)
	case '"':
		return readString(reader)
	case '(':
		nextRune(reader)
		return Token{TokenOpen, ""}, nil
	case ')':
		nextRune(reader)
		return Token{TokenClose, ""}, nil
	default:
		return readSymbol(reader)
	}
}

func tokenize(buf io.RuneReader) TokenReader {
	return TokenReader{buf, false, 0}
}

func _parse(reader *TokenReader) ([]Ast, error) {
	var ast []Ast
	tok, err := nextToken(reader)
	for err == nil && tok.kind != TokenClose {
		if tok.kind == TokenSymbol {
			ast = append(ast, Ast{AstNode, tok.val, []Ast{}})
		} else if tok.kind == TokenOpen {
			astList, err_ := _parse(reader)
			if err_ != nil {
				return ast, err_
			}
			ast = append(ast, Ast{AstList, "", astList})
		}
		tok, err = nextToken(reader)
	}
	return ast, err
}

func parse(buf io.RuneReader) (Ast, error) {
	tokReader := tokenize(buf)
	astList, err := _parse(&tokReader)
	if err != io.EOF && err != nil {
		return Ast{AstList, "", astList}, err
	} else {
		return Ast{AstList, "", astList}, nil
	}
}
