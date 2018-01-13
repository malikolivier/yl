#include "interpreter.hh"

#include <cmath>

const char* UNHANDLED_TYPE_ERROR = "Unhandled type!";

namespace builtins {
	Var printFn(std::vector<Var>& args, ScopeContainer _scope)
	{
		(void) _scope;
		for (const Var& arg: args) {
			std::cout << arg << std::endl;
		}
		return Var();
	}

	Var defFn(std::vector<Var>& args, ScopeContainer _scope)
	{
		(void) args;
		(void) _scope;
		return Var();
	}

	std::vector<std::string> getArgNames(const Ast& args, ScopeContainer scope)
	{
		std::vector<std::string> argNames;
		if (args.type == Ast::VAR) {
			Var res = scope.evaluateVar(args.var);
			argNames.push_back(res.toString());
		} else {
			for (const Ast& arg: args.list) {
				Var res = scope.evaluate(arg);
				argNames.push_back(res.toString());
			}
		}
		return argNames;
	}

	Var defFnCall(const std::vector<Ast>& args, ScopeContainer scope)
	{
		if (args.size() < 2) {
			throw "Function should be used as: '(def name (args...) do...)'";
		}
		std::string identifier = scope.evaluate(args[0]).toString();
		std::vector<std::string> argNames = getArgNames(args[1], scope);
		std::vector<Ast> expr(args.begin() + 2, args.end());
		Ast ast(expr);
		Var func([argNames, scope, ast](std::vector<Var>& fnArgs, ScopeContainer callScope) -> Var {
			(void) callScope;
			ScopeContainer fnScope = scope.extend();
			unsigned int i = 0;
			for (std::string name: argNames) {
				if (i < fnArgs.size()) {
					fnScope.scopePtr->set(name, fnArgs[i]);
				} else {
					fnScope.scopePtr->set(name, Var());
				}
				i++;
			}
			Ast astCopy = ast;
			return fnScope.evaluate(astCopy);
		});
		(*(scope.scopePtr)).set(identifier, func);
		return func;
	}

	Var notOp(std::vector<Var>& args, ScopeContainer _scope)
	{
		(void) _scope;
		if (args.size() < 1) {
			throw "'!' function expects 1 argument";
		}
		if (args[0].type == Var::FALSE) {
			return Var(1);
		} else {
			return Var();
		}
	}
}

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

Var::Var(std::function<Var (std::vector<Var>&, ScopeContainer)> f)
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
		{
			double floored = std::floor(num);
			if (std::abs(floored - num) < 0.000001) {
				return std::to_string((int) floored);
			} else {
				return std::to_string(num);
			}
		}
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

Var Var::call(ScopeContainer scope, std::vector<Ast>& args)
{
	std::vector<Var> fnArgs = scope.getArgs(args);
	switch (type) {
	case FUNCTION:
		return func(fnArgs, scope);
	default:
		throw "Cannot call an uncallable object!";
	}
}

Scope::Scope()
{
	vars = std::unordered_map<std::string, Var>({
		{ "print",  Var(builtins::printFn) },
		{ "def",    Var(builtins::defFn) },
		{ "!",      Var(builtins::notOp) },
	});
}

Scope::Scope(std::shared_ptr<Scope> p)
{
	parent = p;
}

Var& Scope::get(const std::string& name)
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

void Scope::set(std::string name, Var val)
{
	vars.insert({name, val});
}

ScopeContainer::ScopeContainer() : scopePtr(new Scope)
{
}

ScopeContainer::ScopeContainer(std::shared_ptr<Scope> ptr)
{
	scopePtr = ptr;
}

ScopeContainer ScopeContainer::extend() const
{
	std::shared_ptr<Scope> ptr(new Scope(scopePtr));
	return ScopeContainer(ptr);
}

Var ScopeContainer::evaluate(const Ast& ast, bool evaluateFunction /* = true */)
{
	switch (ast.type) {
	case Ast::VAR:
		return evaluateVar(ast.var);
	case Ast::LIST:
		return evaluateList(ast.list, evaluateFunction);
	default:
		throw UNHANDLED_TYPE_ERROR;
	}
}

Var ScopeContainer::evaluateVar(const std::string& str)
{
	try {
		Var var = (*scopePtr).get(str);
		return var;
	} catch (std::out_of_range _) {
		return Var::fromStringToVar(str);
	}
}

Var ScopeContainer::evaluateList(const std::vector<Ast>& ast, bool evaluateFunction)
{
	if (evaluateFunction && ast.size() > 0) {
		if (ast[0].type == Ast::VAR) {
			std::string identifier = ast[0].var;
			if (identifier == "def") {
				std::vector<Ast> args(ast.begin() + 1, ast.end());
				return builtins::defFnCall(args, *this);
			}
			try {
				Var var = scopePtr->get(identifier);
				std::vector<Ast> args(ast.begin() + 1, ast.end());
				return var.call(*this, args);
			} catch (std::out_of_range _) {
				// Fall back to behaviour below
			}
		}
	}
	Var ret = Var();
	for (auto& expr: ast) {
		ret = evaluate(expr);
	}
	return ret;
}

std::vector<Var> ScopeContainer::getArgs(const std::vector<Ast>& args) {
	std::vector<Var> fnArgs;
	for (const Ast& expr: args) {
		fnArgs.push_back(evaluate(expr));
	}
	return fnArgs;
}


Var evaluate(const Ast& ast)
{
	return ScopeContainer().evaluate(ast);
}
