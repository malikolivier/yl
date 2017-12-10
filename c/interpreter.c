#include <stdlib.h>

#include "parser.h"
#include "interpreter.h"

#define CHECK_MEM_ALLOC(ptr) \
	do { \
		if (!ptr) { \
			printf("Memory allocation failed!"); \
			exit(1); \
		} \
	} while(0);

struct YL_Var YL_FALSE = { YL_TYPE_FALSE, { (double) 0 } };
struct YL_Var YL_TRUE = { YL_TYPE_NUMBER, { (double) 1 } };


void varlist_add(struct YL_VarList* varlist, char* id, struct YL_Var* val)
{
	if (!varlist->empty) {
		while(varlist != NULL) {
			if (varlist->tail == NULL) {
				varlist->tail = malloc(sizeof(varlist->tail));
				CHECK_MEM_ALLOC(varlist->tail);
				varlist = varlist->tail;
			}
		}
	}
	varlist->empty = 0;
	varlist->name = id;
	varlist->val = val;
	varlist->tail = NULL;
}

struct YL_Var* scope_set(struct YL_Scope* scope, char* id, struct YL_Var* val)
{
	varlist_add(scope->vars, id, val);
	return val;
}

struct YL_Var* def_fn(char* identifier, int argc, char** arg_names,
                      struct AST* ast, struct YL_Scope* scope)
{
	struct YL_Var* fn = malloc(sizeof(fn));
	CHECK_MEM_ALLOC(fn);
	fn->type = YL_TYPE_FUNC;
	fn->u.func = malloc(sizeof(fn->u.func));
	CHECK_MEM_ALLOC(fn->u.func);
	fn->u.func->argc = argc;
	fn->u.func->builtin = 0;
	fn->u.func->u.ast = ast;
	fn->u.func->scope = scope;
	fn->u.func->arg_names = arg_names;
	scope_set(scope, identifier, fn);
	return fn;
}

struct YL_Var* not_op(int argc, struct YL_Var* argv)
{
	if (argc == 0) {
		return &YL_TRUE;
	}
	if (argv == &YL_FALSE) {
		return &YL_TRUE;
	} else {
		return &YL_FALSE;
	}
}

struct YL_Func DEF_FN = {
	.argc=-1, .builtin=1, .u.builtin_fn=NULL, .arg_names=NULL
};
struct YL_Func NOT_OP = {
	.argc=1, .builtin=1, .u.builtin_fn=not_op, .arg_names=NULL
};
struct YL_Var BUILTIN_VAR_VALS[] = {
	{ .type=YL_TYPE_FUNC, .u.func=(struct YL_Func*) &DEF_FN },
	{ .type=YL_TYPE_FUNC, .u.func=(struct YL_Func*) &NOT_OP }
};
struct YL_VarList NOT_OP_VAR = {
	.name="!", .val=&BUILTIN_VAR_VALS[1], .tail=NULL
};
struct YL_VarList GLOBAL_VARS = {
	.name="def", .val=&BUILTIN_VAR_VALS[0], .tail=&NOT_OP_VAR
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
