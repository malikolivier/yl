#ifndef PARSER_HH
#define PARSER_HH

#include <iostream>
#include <sstream>
#include <string>
#include <vector>

class Ast
{
public:
	enum {
		VAR,
		LIST
	} type;
	std::string var;
	std::vector<Ast> list;

	Ast(std::stringstream& code);
	Ast(std::string symbol);
	Ast(std::vector<Ast> list);
	friend std::ostream& operator<<(std::ostream& os, const Ast& ast);
};

#endif
