#include "interpreter.hh"

#include <cmath>

const char* UNHANDLED_TYPE_ERROR = "Unhandled type!";

namespace builtins {
	Var printFn(std::vector<Var>& args, Scope* _scope)
	{
		(void) _scope;
		for (const Var& arg: args) {
			std::cout << arg << std::endl;
		}
		return Var();
	}

	Var defFn(std::vector<Var>& args, Scope* _scope)
	{
		(void) args;
		(void) _scope;
		return Var();
	}

	std::vector<std::string> getArgNames(Ast& args, Scope* scope)
	{
		std::vector<std::string> argNames;
		if (args.type == Ast::VAR) {
			Var res = scope->evaluateVar(args.var);
			argNames.push_back(res.toString());
		} else {
			for (Ast& arg: *args.list) {
				Var res = scope->evaluate(arg);
				argNames.push_back(res.toString());
			}
		}
		return argNames;
	}

	Var defFnCall(std::vector<Ast>& args, Scope* scope)
	{
		if (args.size() < 2) {
			throw "Function should be used as: '(def name (args...) do...)'";
		}
		std::string identifier = scope->evaluate(args[0]).toString();
		std::vector<std::string> argNames = getArgNames(args[1], scope);
		std::vector<Ast> expr(args.begin() + 2, args.end());
		Ast ast(&expr);
		Var func([argNames, scope, ast](std::vector<Var>& fnArgs, Scope* callScope) -> Var {
			(void) callScope;
			Scope fnScope = scope->extend();
			unsigned int i = 0;
			for (std::string name: argNames) {
				if (i < fnArgs.size()) {
					fnScope.set(name, fnArgs[i]);
				} else {
					fnScope.set(name, Var());
				}
				i++;
			}
			Ast ast_ = ast;
			return fnScope.evaluate(ast_);
		});
		scope->set(identifier, func);
		return func;
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

Var::Var(std::function<Var (std::vector<Var>&, Scope*)> f)
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

Var Var::call(Scope* scope, std::vector<Ast>& args)
{
	std::vector<Var> fnArgs = scope->getArgs(args);
	switch (type) {
	case FUNCTION:
		return func(fnArgs, scope);
	default:
		throw "Cannot call an uncallable object!";
	}
}

Scope::Scope(Scope* p /* = NULL */)
{
	parent = p;
	if (p)
		vars = std::unordered_map<std::string, Var>({});
	else {
		vars = std::unordered_map<std::string, Var>({
			{ "print",  Var(builtins::printFn) },
			{ "def",    Var(builtins::defFn) }
		});
	}
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

void Scope::set(std::string name, Var val)
{
	vars.insert({name, val});
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
		if ((*ast)[0].type == Ast::VAR) {
			std::string identifier = (*ast)[0].var;
			if (identifier == "def") {
				std::vector<Ast> args(ast->begin() + 1, ast->end());
				return builtins::defFnCall(args, this);
			}
			try {
				Var var = get(identifier);
				std::vector<Ast> args(ast->begin() + 1, ast->end());
				return var.call(this, args);
			} catch (std::out_of_range _) {
				// Fall back to behaviour below
			}
		}
	}
	Var ret = Var();
	for (auto& expr: *ast) {
		ret = evaluate(expr);
	}
	return ret;
}

Scope Scope::generateGlobalScope() {
	return Scope();
}

std::vector<Var> Scope::getArgs(std::vector<Ast>& args) {
	std::vector<Var> fnArgs;
	for (Ast& expr: args) {
		fnArgs.push_back(evaluate(expr));
	}
	return fnArgs;
}


Var evaluate(Ast& ast)
{
	return Scope::generateGlobalScope().evaluate(ast);
}
