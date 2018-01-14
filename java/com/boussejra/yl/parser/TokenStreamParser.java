package com.boussejra.yl.parser;

import java.util.ArrayList;

import com.boussejra.yl.parser.Ast;
import com.boussejra.yl.parser.Token;
import com.boussejra.yl.parser.TokenStream;


class TokenStreamParser {
    private TokenStream tokenstream;

    public TokenStreamParser(TokenStream tokenstream) {
        this.tokenstream = tokenstream;
    }

    public Ast parse() throws ParseException {
        ArrayList<Ast> list = new ArrayList<Ast>();
        Token tok = this.tokenstream.next();
        while (tok != null && !tok.isClosedParenthesis()) {
            if (tok.isSymbol()) {
                list.add(new Ast(tok.getSymbol()));
            } else if (tok.isOpenParenthesis()) {
                list.add(this.parse());
            }
            tok = this.tokenstream.next();
        }
        return new Ast(list);
    }
}
