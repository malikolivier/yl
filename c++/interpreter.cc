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

	Var letFn(std::vector<Var>& args, ScopeContainer scope)
	{
		if (args.size() < 1) {
			throw "'let' function should be used as: '(let name val)'";
		}
		std::string identifier = args[0].toString();
		Var rhs = Var();
		if (args.size() > 1) {
			rhs = args[1];
		}
		scope.scopePtr->set(identifier, rhs);
		return rhs;
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
			return fnScope.evaluate(ast, false);
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
		return Var::fromBool(args[0].type == Var::FALSE);
	}

	Var eqOp(std::vector<Var>& args, ScopeContainer _scope)
	{
		(void) _scope;
		if (args.size() < 2) {
			throw "'=' function expects 2 argument";
		}
		return Var::fromBool(args[0] == args[1]);
	}

	Var gtOp(std::vector<Var>& args, ScopeContainer _scope)
	{
		(void) _scope;
		if (args.size() < 2) {
			throw "'>' function expects 2 argument";
		}
		return Var::fromBool(args[0] > args[1]);
	}

	Var geOp(std::vector<Var>& args, ScopeContainer _scope)
	{
		(void) _scope;
		if (args.size() < 2) {
			throw "'>=' function expects 2 argument";
		}
		return Var::fromBool(args[0] >= args[1]);
	}

	Var ltOp(std::vector<Var>& args, ScopeContainer _scope)
	{
		(void) _scope;
		if (args.size() < 2) {
			throw "'<' function expects 2 argument";
		}
		return Var::fromBool(args[0] < args[1]);
	}

	Var leOp(std::vector<Var>& args, ScopeContainer _scope)
	{
		(void) _scope;
		if (args.size() < 2) {
			throw "'<=' function expects 2 argument";
		}
		return Var::fromBool(args[0] <= args[1]);
	}

	Var plusOp(std::vector<Var>& args, ScopeContainer _scope)
	{
		(void) _scope;
		if (args.size() < 1) {
			throw "'+' function expects at least one argument";
		}
		Var var = args[0];
		std::vector<Var> nextArgs(args.begin() + 1, args.end());
		for (const Var& arg: nextArgs) {
			var = var + arg;
		}
		return var;
	}

	Var minusOp(std::vector<Var>& args, ScopeContainer _scope)
	{
		(void) _scope;
		if (args.size() < 2) {
			throw "'-' function expects 2 argument";
		}
		return args[0] - args[1];
	}

	Var multiplyOp(std::vector<Var>& args, ScopeContainer _scope)
	{
		(void) _scope;
		if (args.size() < 2) {
			throw "'*' function expects 2 argument";
		}
		return args[0] * args[1];
	}

	Var divideOp(std::vector<Var>& args, ScopeContainer _scope)
	{
		(void) _scope;
		if (args.size() < 2) {
			throw "'/' function expects 2 argument";
		}
		return args[0] / args[1];
	}

	Var moduloOp(std::vector<Var>& args, ScopeContainer _scope)
	{
		(void) _scope;
		if (args.size() < 2) {
			throw "'%' function expects 2 argument";
		}
		return args[0] % args[1];
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

Var Var::fromBool(bool b)
{
	if (b) {
		return Var(1);
	} else {
		return Var();
	}
}

Var Var::fromString(std::string str)
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

bool operator==(const Var& var1, const Var& var2)
{
	switch (var1.type) {
	case Var::FALSE:
		return var2.type == Var::FALSE;
	case Var::NUMBER:
		if (var2.type == Var::NUMBER)
			return std::abs(var1.num - var2.num) < 0.000001;
		else
			return false;
	case Var::STRING:
		if (var2.type == Var::STRING)
			return var1.str == var2.str;
		else
			return false;
	case Var::FUNCTION:
		if (var2.type == Var::FUNCTION) {
			Var (*const* funcPtr1)(std::vector<Var>&, ScopeContainer) = var1.func.target<Var(*)(std::vector<Var>&, ScopeContainer)>();
			Var (*const* funcPtr2)(std::vector<Var>&, ScopeContainer) = var2.func.target<Var(*)(std::vector<Var>&, ScopeContainer)>();
			return funcPtr1 == funcPtr2;
		} else
			return false;
	default:
		return UNHANDLED_TYPE_ERROR;
	}
}

bool operator<(const Var& var1, const Var& var2)
{
	switch (var1.type) {
	case Var::FALSE:
		return var2.type != Var::FALSE;
	case Var::NUMBER:
		if (var2.type == Var::NUMBER)
			return var1.num < var2.num;
		else
			return var2.type != Var::FALSE;
	case Var::STRING:
		if (var2.type == Var::STRING)
			return var1.str < var2.str;
		else
			return var2.type == Var::FUNCTION;
	case Var::FUNCTION:
		return var2.type != Var::FUNCTION;
	default:
		return UNHANDLED_TYPE_ERROR;
	}
}

bool operator<=(const Var& var1, const Var& var2)
{
	return var1 < var2 || var1 == var2;
}

bool operator>(const Var& var1, const Var& var2)
{
	return !(var1 <= var2);
}

bool operator>=(const Var& var1, const Var& var2)
{
	return var1 > var2 || var1 == var2;
}

Var operator+(const Var& var1, const Var& var2)
{
	switch (var1.type) {
	case Var::NUMBER:
		if (var2.type == Var::NUMBER) {
			return Var(var1.num + var2.num);
		} else if (var2.type == Var::STRING) {
			Var var1s(var1.toString());
			return Var(var1s.str + var2.str);
		}
		break;
	case Var::STRING:
		if (var2.type == Var::NUMBER) {
			Var var2s(var2.toString());
			return Var(var1.str + var2s.str);
		} else if (var2.type == Var::STRING) {
			return Var(var1.str + var2.str);
		}
		break;
	default:
		{} // Fall through
	}
	throw "Cannot only add number or strings";
}

Var operator-(const Var& var1, const Var& var2)
{
	if (var1.type == Var::NUMBER && var2.type == Var::NUMBER) {
		return Var(var1.num - var2.num);
	} else {
		throw "Cannot substract non-numerals";
	}
}

Var operator*(const Var& var1, const Var& var2)
{
	if (var1.type == Var::NUMBER && var2.type == Var::NUMBER) {
		return Var(var1.num * var2.num);
	} else {
		throw "Cannot multiply non-numerals";
	}
}

Var operator/(const Var& var1, const Var& var2)
{
	if (var1.type == Var::NUMBER && var2.type == Var::NUMBER) {
		return Var(var1.num / var2.num);
	} else {
		throw "Cannot divide non-numerals";
	}
}

Var operator%(const Var& var1, const Var& var2)
{
	if (var1.type == Var::NUMBER && var2.type == Var::NUMBER) {
		return Var(std::fmod(var1.num, var2.num));
	} else {
		throw "Cannot take the remainder on non-numerals";
	}
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
		{ "let",    Var(builtins::letFn) },
		{ "def",    Var(builtins::defFn) },
		{ "!",      Var(builtins::notOp) },
		{ "=",      Var(builtins::eqOp) },
		{ ">",      Var(builtins::gtOp) },
		{ ">=",     Var(builtins::geOp) },
		{ "<",      Var(builtins::ltOp) },
		{ "<=",     Var(builtins::leOp) },
		{ "+",      Var(builtins::plusOp) },
		{ "-",      Var(builtins::minusOp) },
		{ "*",      Var(builtins::multiplyOp) },
		{ "/",      Var(builtins::divideOp) },
		{ "%",      Var(builtins::moduloOp) },
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
		return Var::fromString(str);
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
