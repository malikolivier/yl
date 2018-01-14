package com.boussejra.yl.parser;

import com.boussejra.yl.YlException;

@SuppressWarnings("serial")
public class ParseException extends YlException {
    public ParseException(String message) {
        super(message);
    }
}
