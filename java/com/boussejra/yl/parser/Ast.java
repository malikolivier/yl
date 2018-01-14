package com.boussejra.yl.parser;

import com.boussejra.yl.parser.AstType;
import com.boussejra.yl.parser.InputStream;
import com.boussejra.yl.parser.ParseException;
import com.boussejra.yl.parser.TokenStream;
import com.boussejra.yl.parser.TokenStreamParser;

import java.util.ArrayList;


public class Ast {
    private AstType type;
    private String sym;
    private ArrayList<Ast> list;

    public static Ast parseCode(String code) throws ParseException {
        InputStream inputstream = new InputStream(code);
        TokenStream tokstream = new TokenStream(inputstream);
        TokenStreamParser parser = new TokenStreamParser(tokstream);
        return parser.parse();
    }

    public Ast(String sym) {
        this.type = AstType.NODE;
        this.sym = sym;
        this.list = null;
    }

    public Ast(ArrayList<Ast> list) {
        this.type = AstType.LIST;
        this.sym = null;
        this.list = list;
    }

    public AstType getType() {
        return this.type;
    }

    public String getSym() {
        return this.sym;
    }

    public ArrayList<Ast> getList() {
        return this.list;
    }

    @Override
    public String toString() {
        String out = new String();
        if (this.type == AstType.NODE) {
            out += "\"" + this.sym + "\"";
        } else if (this.type == AstType.LIST) {
            for (Ast ast: this.list) {
                out += " (" + ast.toString() + ") ";
            }
        }
        return out;
    }
}
