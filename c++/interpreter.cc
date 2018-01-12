#include "interpreter.hh"

Var::Var()
{
	type = FALSE;
}

Var::Var(double n)
{
	type = NUMBER;
	num = n;
}

Var::Var(std::string s)
{
	type = STRING;
	str = s;
}

Var::Var(Var (*f)(std::vector<Var&>, Scope*))
{
	type = FUNCTION;
	func = f;
}

int Var::toInt() const
{
	if (type == NUMBER) {
		return num;
	} else {
		return 0;
	}
}

std::string Var::toString() const
{
	switch (type) {
	case FALSE:
		return "()";
	case NUMBER:
		return std::to_string(num);
	case STRING:
		return str;
	case FUNCTION:
		return "(def function (args...) do...)";
	default:
		throw "Unhandled type!";
	}
}

std::ostream& operator<<(std::ostream& os, const Var& var)
{
	os << var.toString();
	return os;
}

Scope::Scope(Scope* p /* = NULL */)
{
	parent = p;
	std::unordered_map<std::string, Var> vars();
}

Scope Scope::extend()
{
	return Scope(this);
}

Var& Scope::get(std::string name)
{
	try {
		return vars.at(name);
	} catch (std::out_of_range e) {
		if (parent) {
			return parent->get(name);
		} else {
			throw e;
		}
	}
}

Var& Scope::set(std::string name, Var& val)
{
	vars.insert({name, val});
	return val;
}

Var Scope::evaluate(Ast& ast, bool evaluateFunction /* = true */)
{
	// TODO
	(void) ast;
	(void) evaluateFunction;
	return Var();
}

Scope Scope::generateGlobalScope() {
	Scope scope;
	return scope;
}

Var evaluate(Ast& ast)
{
	return Scope::generateGlobalScope().evaluate(ast);
}
