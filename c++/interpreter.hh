#ifndef INTERPRETER_HH
#define INTERPRETER_HH

#include <string>
#include <unordered_map>
#include <vector>

#include "parser.hh"

class Scope;

class Var
{
public:
	enum {
		FALSE,
		NUMBER,
		STRING,
		FUNCTION
	} type;
	double num;
	std::string str;
	Var (*func)(std::vector<Var*>, Scope*);

	Var();
	Var(double);
	Var(std::string);
	Var(Var (*)(std::vector<Var*>, Scope*));

	int toInt() const;
	std::string toString() const;
	friend std::ostream& operator<<(std::ostream& os, const Var& var);
};

class Scope
{
public:
	Scope* parent;
	std::unordered_map<std::string, Var> vars;

	Scope(Scope* = NULL);

	Scope extend() const;
	Var* get(std::string) const;
	Var* set(std::string, Var);

	Var evaluate(Ast&, bool = true);

	static Scope generateGlobalScope();
};

Var evaluate(Ast&);

#endif
