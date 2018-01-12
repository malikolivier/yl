#include "interpreter.hh"

const char* UNHANDLED_TYPE_ERROR = "Unhandled type!";

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

Var Var::fromStringToVar(std::string str)
{
	try {
		double n = std::stod(str);
		return Var(n);
	} catch (std::invalid_argument _) {
		return Var(str);
	}
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
		throw UNHANDLED_TYPE_ERROR;
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
	switch (ast.type) {
	case Ast::VAR:
		return Scope::evaluateVar(ast.var);
	case Ast::LIST:
		return Scope::evaluateList(ast.list, evaluateFunction);
	default:
		throw UNHANDLED_TYPE_ERROR;
	}
}

Var Scope::evaluateVar(std::string str)
{
	try {
		Var var = get(str);
		return var;
	} catch (std::out_of_range _) {
		return Var::fromStringToVar(str);
	}
}

Var Scope::evaluateList(std::vector<Ast>* ast, bool evaluateFunction)
{
	if (evaluateFunction && ast->size() > 0) {

	}
	Var ret = Var();
	for (auto& expr: *ast) {
		ret = evaluate(expr);
	}
	return ret;
}

Scope Scope::generateGlobalScope() {
	Scope scope;
	return scope;
}

Var evaluate(Ast& ast)
{
	return Scope::generateGlobalScope().evaluate(ast);
}
