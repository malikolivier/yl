class InputStream:
    def __init__(self, _input):
        self.input = _input
        self.pos = 0
        self.line = 1
        self.col = 0

    def next(self):
        ch = self.input[self.pos]
        self.pos += 1
        if ch == '\n':
            self.line += 1
            self.col = 0
        else:
            self.col += 1
        return ch

    def peek(self):
        if self.pos < len(self.input):
            return self.input[self.pos]

    def eof(self):
        return self.peek() is None

    def croak(self, msg):
        raise Exception('%s (%d:%d)' %(msg, self.line, self.col))


class TokenStream:
    def __init__(self, _input):
        self.input = _input
        self.current = None
        self.croak = self.input.croak

    def _is_whitespace(self, ch):
        return ch == ' ' or ch == '\t' or ch == '\n'

    def _is_parenthesis(self, ch):
        return ch == '(' or ch == ')'

    def _is_symbol(self, ch):
        return (not self._is_whitespace(ch)) and (not self._is_parenthesis(ch))

    def _read_while(self, predicate):
        string = ''
        while (not self.input.eof() and predicate(self.input.peek())):
            string += self.input.next()
        return string

    def _read_escaped(self, end):
        escaped = False
        string = ''
        self.input.next()
        while not self.input.eof():
            ch = self.input.next()
            if escaped:
                string += ch
                escaped = False
            elif ch == '\\':
                escaped = True
            elif ch == end:
                break
            else:
                string += ch
        return string

    def _read_string(self):
        return {'type': 'symbol', 'value': self._read_escaped('"')}

    def _read_symbol(self):
        identifier = self._read_while(self._is_symbol);
        return {'type': 'symbol', 'value': identifier}

    def _skip_comment(self):
        self._read_while(lambda ch: ch != '\n')
        self.input.next()

    def _read_next(self):
        self._read_while(self._is_whitespace)
        if self.input.eof():
            return None
        ch = self.input.peek()
        if ch == ';':
            self._skip_comment()
            return self._read_next()
        if ch == '"':
            return self._read_string()
        if self._is_symbol(ch):
            return self._read_symbol()
        if self._is_parenthesis(ch):
            return {'type': 'punc', 'value': self.input.next()}
        self.input.croak("Can't handler character %c" % ch)

    def peek(self):
        if self.current:
            return self.current
        else:
            self.current = self._read_next()
            return self.current

    def next(self):
        tok = self.current
        self.current = None
        return tok or self._read_next()

    def eof(self):
        return self.peek() is None


def _parse(_input):
    ast = []
    tok = _input.next()
    while tok is not None and not (tok['type'] == 'punc' and
                                   tok['value'] == ')'):
        if tok['type'] == 'symbol':
            ast.append(tok['value'])
        elif tok['type'] == 'punc' and tok['value'] == '(':
            ast.append(_parse(_input))
        tok = _input.next()
    return ast


def parse(code):
    return _parse(TokenStream(InputStream(code)));
