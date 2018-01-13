#ifndef INTERPRETER_HH
#define INTERPRETER_HH

#include <functional>
#include <memory>
#include <string>
#include <unordered_map>
#include <vector>

#include "parser.hh"

class Scope;
class ScopeContainer;

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
	std::function<Var (std::vector<Var>&, ScopeContainer)> func;

	Var();
	Var(double);
	Var(std::string);
	Var(std::function<Var (std::vector<Var>&, ScopeContainer)>);

	static Var fromBool(bool);
	static Var fromString(std::string);
	int toInt() const;
	std::string toString() const;
	friend std::ostream& operator<<(std::ostream& os, const Var& var);
	friend bool operator==(const Var&, const Var&);
	friend bool operator<(const Var&, const Var&);
	friend bool operator<=(const Var&, const Var&);
	friend bool operator>(const Var&, const Var&);
	friend bool operator>=(const Var&, const Var&);

	Var call(ScopeContainer, std::vector<Ast>& args);
};

class Scope
{
public:
	std::shared_ptr<Scope> parent;
	std::unordered_map<std::string, Var> vars;

	Scope();
	Scope(std::shared_ptr<Scope>);

	Var& get(const std::string&);
	void set(std::string, Var);
};

class ScopeContainer
{
public:
	std::shared_ptr<Scope> scopePtr;

	ScopeContainer();
	ScopeContainer(std::shared_ptr<Scope>);
	ScopeContainer extend() const;

	Var evaluate(const Ast&, bool = true);
	Var evaluateVar(const std::string&);
	Var evaluateList(const std::vector<Ast>&, bool);

	std::vector<Var> getArgs(const std::vector<Ast>& args);
};

Var evaluate(const Ast&);

#endif
