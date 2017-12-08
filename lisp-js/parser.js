/*
 * AST
 * {
 *   type: "list",
 *   tokens: [
 *      AST,
 *      ...
 *   ]
 * }
 * OR
 * "string"
 *
 * Example:
 *
 * (print 1)
 *
 * {
 *   type: "list",
 *   tokens: [
 *       {
 *           type: "list",
 *           tokens: [
 *               'print',
 *               '1'
 *           ]
 *       }
 *   ]
 * }
 */

function InputStream(input) {
    var pos = 0, line = 1, col = 0;
    return {
        next  : next,
        peek  : peek,
        eof   : eof,
        croak : croak,
    };
    function next() {
        var ch = input.charAt(pos++);
        if (ch == "\n") line++, col = 0; else col++;
        return ch;
    }
    function peek() {
        return input.charAt(pos);
    }
    function eof() {
        return peek() == "";
    }
    function croak(msg) {
        throw new Error(msg + " (" + line + ":" + col + ")");
    }
}

function TokenStream(input) {
    var current = null;
    return {
        next  : next,
        peek  : peek,
        eof   : eof,
        croak : input.croak
    };
    function is_whitespace(ch) {
        return " \t\n".indexOf(ch) >= 0;
    }
    function is_symbol(ch) {
        return !is_parenthesis(ch) && !is_whitespace(ch);
    }
    function is_parenthesis(ch) {
        return "()".indexOf(ch) >= 0;
    }
    function read_while(predicate) {
        var str = "";
        while (!input.eof() && predicate(input.peek()))
            str += input.next();
        return str;
    }
    function read_escaped(end) {
        var escaped = false, str = "";
        input.next();
        while (!input.eof()) {
            var ch = input.next();
            if (escaped) {
                str += ch;
                escaped = false;
            } else if (ch == "\\") {
                escaped = true;
            } else if (ch == end) {
                break;
            } else {
                str += ch;
            }
        }
        return str;
    }
    function read_string() {
        return { type: "symbol", value: read_escaped('"') };
    }
    function read_symbol() {
        var id = read_while(is_symbol);
        return {
            type  : "symbol",
            value : id
        };
    }
    function skip_comment() {
        read_while(function(ch){ return ch != "\n" });
        input.next();
    }
    function read_next() {
        read_while(is_whitespace);
        if (input.eof()) return null;
        var ch = input.peek();
        if (ch == ";") {
            skip_comment();
            return read_next();
        }
        if (ch == '"') return read_string();
        if (is_symbol(ch)) return read_symbol();
        if (is_parenthesis(ch)) return {
            type  : "punc",
            value : input.next()
        };
        input.croak("Can't handle character: " + ch);
    }
    function peek() {
        return current || (current = read_next());
    }
    function next() {
        var tok = current;
        current = null;
        return tok || read_next();
    }
    function eof() {
        return peek() == null;
    }
}

function Parser(input) {
    return parse_expression();
    function unexpected() {
        input.croak("Unexpected token: " + JSON.stringify(input.peek()));
    }
    function parse_expression() {
        var list = []
        do {
            var tok = input.next();
            if (tok === null) {
                break;
            } else if (tok.type === 'symbol') {
                list.push(tok.value);
            } else if (tok.type === "punc" && tok.value === '(') {
                list.push(parse_expression());
            }
        } while (!(tok.type === "punc" && tok.value === ')'));
        return list;
    }
}

function parse(code) {
    return Parser(TokenStream(InputStream(code)));
}

module.exports = parse;
