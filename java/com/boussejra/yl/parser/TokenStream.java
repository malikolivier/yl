package com.boussejra.yl.parser;

import com.boussejra.yl.parser.InputStream;
import com.boussejra.yl.parser.ParseException;
import com.boussejra.yl.parser.Token;
import com.boussejra.yl.parser.TokenType;

import java.util.function.Predicate;

public class TokenStream {
    private InputStream input;
    private static Predicate<Character> isNotNewLine = c -> c != '\n';
    private static Predicate<Character> isWhitespace = c -> c == ' ' || c == '\t' || c == '\n';
    private static Predicate<Character> isParenthesis = c -> c == '(' || c == ')';
    private static Predicate<Character> isSymbol = c -> !isWhitespace.test(c) && !isParenthesis.test(c);

    public TokenStream(InputStream input) {
        this.input = input;
    }

    public Token next() throws ParseException {
        this.readWhile(isWhitespace);
        if (this.input.eof()) {
            return null;
        }
        char c = this.input.peek();
        if (c == ';') {
            this.skipComment();
            return this.next();
        }
        if (c == '"') {
            return this.readString();
        }
        if (isSymbol.test(c)) {
            return this.readSymbol();
        }
        if (isParenthesis.test(c)) {
            return this.readParenthesis();
        }
        throw new ParseException("Unhandled character in input stream");
    }

    private String readWhile(Predicate<Character> predicate) {
        StringBuilder sb = new StringBuilder();
        while (!this.input.eof() && predicate.test(this.input.peek())) {
            sb.append(this.input.next());
        }
        return sb.toString();
    }

    private String readEscaped() {
        boolean escaped = false;
        StringBuilder sb = new StringBuilder();
        this.input.next();
        while (!this.input.eof()) {
            char c = this.input.next();
            if (escaped) {
                sb.append(c);
                escaped = false;
            } else if (c == '\\') {
                escaped = true;
            } else if (c == '"') {
                break;
            } else {
                sb.append(c);
            }
        }
        return sb.toString();
    }

    private Token readString() {
        return new Token(this.readEscaped());
    }

    private Token readSymbol() {
        return new Token(this.readWhile(isSymbol));
    }

    private Token readParenthesis() {
        char c = this.input.next();
        if (c == '(') {
            return new Token(TokenType.OPEN);
        } else {
            return new Token(TokenType.CLOSE);
        }
    }

    private void skipComment() {
        this.readWhile(isNotNewLine);
        this.input.next();
    }
}
