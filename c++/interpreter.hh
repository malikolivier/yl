#ifndef INTERPRETER_HH
#define INTERPRETER_HH

#include <functional>
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
	std::function<Var (std::vector<Var>&, Scope*)> func;

	Var();
	Var(double);
	Var(std::string);
	Var(std::function<Var (std::vector<Var>&, Scope*)>);

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
	void set(std::string, Var);

	Var evaluate(const Ast&, bool = true);
	Var evaluateVar(const std::string&);
	Var evaluateList(const std::vector<Ast>&, bool);

	static Scope generateGlobalScope();

	std::vector<Var> getArgs(const std::vector<Ast>& args);
};

Var evaluate(const Ast&);

#endif
