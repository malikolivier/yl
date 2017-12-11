#include <stdlib.h>
#include <string.h>

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

struct YL_Var* varlist_find(struct YL_VarList* varlist, char* id)
{
	if (varlist == NULL || varlist->empty) {
		return NULL;
	} else {
		if (strcmp(varlist->name, id) == 0) {
			return varlist->val;
		} else {
			return varlist_find(varlist->tail, id);
		}
	}
}

struct YL_Var* scope_set(struct YL_Scope* scope, char* id, struct YL_Var* val)
{
	varlist_add(scope->vars, id, val);
	return val;
}

struct YL_Var* scope_get(struct YL_Scope* scope, char* id)
{
	struct YL_Var* val = NULL;
	if (scope == NULL) {
		return NULL;
	} else {
		val = varlist_find(scope->vars, id);
		if (val) {
			return val;
		} else {
			return scope_get(scope->parent, id);
		}
	}
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

int is_number(char* str)
{
	char** endptr = &str;
	double out = strtod(str, endptr);
	if (out == 0 && &str == endptr) {
		return 0;
	} else {
		return **endptr == '\0';
	}
}

int ast_len(struct AST* ast) {
	if (!ast)
		return 0;
	switch(ast->type) {
	case AST_EMPTY:
		return 0;
	case AST_VAL:
		return 1 + ast_len(ast->tail);
	case AST_LIST:
		return ast_len(ast->val.ast) + ast_len(ast->tail);
	}
}

struct YL_Var* yl_evaluate_in_scope(struct AST* ast, struct YL_Scope* scope,
                                    int evaluate_function)
{
	struct YL_Var* ret;
	switch(ast->type) {
	case AST_EMPTY:
		return &YL_FALSE;
	case AST_VAL:
		ret = scope_get(scope, ast->val.tok);
		if (ret) {
			return ret;
		} else if(is_number(ast->val.tok)) {
			ret = malloc(sizeof(ret));
			ret->type = YL_TYPE_NUMBER;
			ret->u.num = strtod(ast->val.tok, NULL);
			return ret;
		} else {
			ret = malloc(sizeof(ret));
			ret->type = YL_TYPE_STRING;
			ret->u.str = ast->val.tok;
			return ret;
		}
	case AST_LIST:
		if (evaluate_function && ast_len(ast) > 0 &&
		    ast->val.ast->type == AST_VAL &&
		    scope_get(scope, ast->val.ast->val.tok) != NULL) {
			char* fn_name = ast->val.ast->val.tok;
			if (strcmp(fn_name, "def") == 0) {
				/* TODO  Call def_fn */
			} else {
				/* Run a normal function */
				/* TODO */
			}
		}
	}
	return &YL_FALSE;
}


struct YL_Var* yl_evaluate(struct AST* ast)
{
	return yl_evaluate_in_scope(ast, &GLOBAL_SCOPE, 1);
}

void yl_print(struct YL_Var* obj)
{
	/* TODO */
	(void)obj;
}
