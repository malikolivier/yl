#ifndef PARSER_HH
#define PARSER_HH

#include <iostream>
#include <sstream>
#include <string>
#include <vector>

class Ast
{
	enum {
		VAR,
		LIST
	} type;
	union {
		std::string* var;
		std::vector<Ast>* list;
	};
public:
	Ast(const std::stringstream& code);
	~Ast();
	friend std::ostream& operator<<(std::ostream& os, const Ast& ast);
};

#endif
