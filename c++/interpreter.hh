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
	Var (*func)(std::vector<Var>, Scope*);

	Var();
	Var(double);
	Var(std::string);
	Var(Var (*)(std::vector<Var>, Scope*));

	static Var fromStringToVar(std::string);
	int toInt() const;
	std::string toString() const;
	friend std::ostream& operator<<(std::ostream& os, const Var& var);

	Var call(Scope*, std::vector<Ast>& args);
};

class Scope
{
public:
	Scope* parent;
	std::unordered_map<std::string, Var> vars;

	Scope(Scope* = NULL);

	Scope extend();
	Var& get(std::string);
	Var& set(std::string, Var&);

	Var evaluate(Ast&, bool = true);
	Var evaluateVar(std::string);
	Var evaluateList(std::vector<Ast>*, bool);

	static Scope generateGlobalScope();

	std::vector<Var> getArgs(std::vector<Ast>& args);
};

Var evaluate(Ast&);

#endif
