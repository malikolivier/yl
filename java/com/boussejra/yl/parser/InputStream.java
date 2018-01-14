package com.boussejra.yl.parser;

public class InputStream {
    private int pos = 0;
    private String input;

    public InputStream(String input) {
        this.input = input;
    }

    public char next() {
        char c = this.peek();
        this.pos++;
        return c;
    }

    public char peek() {
        return this.input.charAt(this.pos);
    }

    public boolean eof() {
        return this.pos >= this.input.length();
    }
}
