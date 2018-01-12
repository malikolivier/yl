#include "parser.hh"

Ast::Ast(std::string symbol) {
	type = VAR;
	var = symbol;
}

Ast::Ast(std::vector<Ast>* list_) {
	type = LIST;
	list = list_;
}

class InputStream
{
	std::stringstream* code;
public:
	InputStream(std::stringstream* code_)
	{
		code = code_;
	}

	char next()
	{
		return code->get();
	}

	char peek()
	{
		return code->peek();
	}

	bool eof()
	{
		return peek() == EOF;
	}
};

class Token
{
public:
	enum TokenType {
		Open,
		Close,
		Symbol
	} type;
	std::string symbol;

	Token(TokenType type_)
	{
		type = type_;
	}
	Token(std::string symbol_)
	{
		type = Symbol;
		symbol = symbol_;
	}
};

bool isWhitespace(char ch)
{
	return ch == ' ' || ch == '\t' || ch == '\n';
}

bool isParenthesis(char ch) {
	return ch == '(' || ch == ')';
}

bool isSymbol(char ch)
{
	return !isParenthesis(ch) && !isWhitespace(ch);
}

class TokenStream
{
	InputStream* input;

	std::string readWhile(bool (*predicate)(char))
	{
		std::string str;
		while (!input->eof() && predicate(input->peek())) {
			str += input->next();
		}
		return str;
	}

	void skipComment()
	{
		readWhile([](char c) {
			return c != '\n';
		});
		input->next();
	}

	std::string readEscaped()
	{
		bool escaped = false;
		std::string str;
		input->next();
		while (!input->eof()) {
			char c = input->next();
			if (escaped) {
				str += c;
				escaped = false;
			} else if (c == '\\') {
				escaped = true;
			} else if (c == '"') {
				break;
			} else {
				str += c;
			}
		}
		return str;
	}

	Token* readString()
	{
		std::string str = readEscaped();
		return new Token(str);
	}

	Token* readSymbol()
	{
		std::string str = readWhile(isSymbol);
		return new Token(str);
	}

	Token* readParenthesis()
	{
		char c = input->next();
		if (c == '(') {
			return new Token(Token::Open);
		} else {
			return new Token(Token::Close);
		}
	}

	Token* next()
	{
		readWhile(isWhitespace);
		if (input->eof()) {
			return NULL;
		}
		char c = input->peek();
		if (c == ';') {
			skipComment();
			return next();
		}
		if (c == '"') {
			return readString();
		}
		if (isSymbol(c)) {
			return readSymbol();
		}
		if (isParenthesis(c)) {
			return readParenthesis();
		}
		throw "I should not be here";
	}
public:
	TokenStream(std::stringstream* code)
	{
		input = new InputStream(code);
	}

	std::vector<Ast>* toAst()
	{
		std::vector<Ast>* ast = new std::vector<Ast>();
		Token* tok = next();
		while (tok != NULL && tok->type != Token::Close) {
			if (tok->type == Token::Symbol) {
				Ast* val = new Ast(tok->symbol);
				ast->push_back(*val);
			} else if (tok->type == Token::Open) {
				Ast* val = new Ast(toAst());
				ast->push_back(*val);
			}
			tok = next();
		}
		return ast;
	}
};


std::vector<Ast>* parseCode(std::stringstream& code)
{
	TokenStream tokenStream(&code);
	return tokenStream.toAst();
}


Ast::Ast(std::stringstream& code)
{
	type = LIST;
	list = parseCode(code);
}

Ast::~Ast()
{
	if (type == LIST) {
		delete list;
	}
}

std::ostream& operator<<(std::ostream& os, const Ast& ast)
{
	if (ast.type == Ast::VAR) {
		os << '"' << ast.var << "\" ";
	} else {
		os << " (";
		for (auto const& value: *ast.list) {
			os << value;
		}
		os << ") ";
	}
	return os;
}
