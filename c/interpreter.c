#include "parser.h"
#include "interpreter.h"

struct YL_Var YL_FALSE = { YL_TYPE_FALSE, { (double) 0 } };

struct YL_Var* def_fn(int argc, struct YL_Var* argv)
{
	/* TODO */
	(void)argc;
	(void)argv;
	return &YL_FALSE;
}

struct YL_Func_2 DEF_FN = {
	.argc=2, .builtin=1, .u.builtin_fn=def_fn, .arg_names={ "lhs", "rhs" },
};
struct YL_Var BUILTIN_VAR_VALS[] = {
	{ .type=YL_TYPE_FUNC, .u.func=(struct YL_Func*) &DEF_FN }
};
struct YL_VarList GLOBAL_VARS = {
	.name="def", .val=&BUILTIN_VAR_VALS[0], .tail=NULL
};
struct YL_Scope GLOBAL_SCOPE = { .vars=&GLOBAL_VARS, .parent=NULL };

struct YL_Var* yl_evaluate_in_scope(struct AST* ast, struct YL_Scope* scope)
{
	/* TODO */
	(void)ast;
	(void)scope;
	return &YL_FALSE;
}


struct YL_Var* yl_evaluate(struct AST* ast)
{
	return yl_evaluate_in_scope(ast, &GLOBAL_SCOPE);
}

void yl_print(struct YL_Var* obj)
{
	/* TODO */
	(void)obj;
}
