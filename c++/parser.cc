#include "parser.hh"

Ast::Ast(const std::stringstream& code) {
	// TODO
	type = VAR;
	var = new std::string("test");
}

Ast::~Ast() {
	if (type == VAR) {
		delete var;
	} else {
		delete list;
	}
}

std::ostream& operator<<(std::ostream& os, const Ast& ast) {
	if (ast.type == Ast::VAR) {
		os << '"' << *ast.var << "\" ";
	} else {
		os << " (";
		for (auto const& value: *ast.list) {
			os << value;
		}
		os << ") ";
	}
	return os;
}
