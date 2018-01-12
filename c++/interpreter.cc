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

Var::Var(Var (*f)(std::vector<Var*>, Scope*))
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
