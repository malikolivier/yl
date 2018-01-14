package com.boussejra.yl.parser;

import com.boussejra.yl.parser.TokenType;

public class Token {
    private TokenType type;
    private String sym;

    public Token(TokenType type) {
        this.type = type;
    }

    public Token(String sym) {
        this.type = TokenType.SYMBOL;
        this.sym = sym;
    }

    public boolean isOpenParenthesis() {
        return this.type == TokenType.OPEN;
    }

    public boolean isClosedParenthesis() {
        return this.type == TokenType.CLOSE;
    }

    public boolean isSymbol() {
        return this.type == TokenType.SYMBOL;
    }

    public String getSymbol() {
        return this.sym;
    }

    @Override
    public String toString() {
        if (type == TokenType.OPEN) {
            return "(";
        } else if (type == TokenType.OPEN) {
            return ")";
        } else {
            return this.sym;
        }
    }
}
