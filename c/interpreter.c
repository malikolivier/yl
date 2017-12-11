#include <stdlib.h>
#include <string.h>
#include <math.h>

#include "parser.h"
#include "interpreter.h"

#define CHECK_MEM_ALLOC(ptr) \
	do { \
		if (!ptr) { \
			CROAK("Memory allocation failed!"); \
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

struct YL_Scope* scope_extend(struct YL_Scope* scope)
{
	struct YL_Scope* child = malloc(sizeof(child));
	CHECK_MEM_ALLOC(child);
	child->parent = scope;
	child->vars = malloc(sizeof(child->vars));
	CHECK_MEM_ALLOC(child->vars);
	child->vars->empty = 1;
	return child;
}

/* Prototypes */
int ast_len(struct AST*);
char* cast_yl_var_to_string(struct YL_Var* var);

void ast_get_names(struct AST* ast, struct YL_Scope* scope, char** names)
{
	int i = 0;
	while(ast != NULL && ast->type != AST_EMPTY) {
		struct YL_Var* var = yl_evaluate_in_scope(ast, scope, 0);
		names[i] = cast_yl_var_to_string(var);
		i++;
	}
}

void ast_extract_argv(struct AST* ast, struct YL_Scope* scope,
                      struct YL_Var** argv)
{
	int i = 0;
	while(ast != NULL && ast->type != AST_EMPTY) {
		argv[i] = yl_evaluate_in_scope(ast, scope, 1);
		i++;
	}
}

struct YL_Var* def_fn(char* identifier, struct AST* args, struct YL_Scope* scope)
{
	struct YL_Var* fn = malloc(sizeof(fn));
	CHECK_MEM_ALLOC(fn);
	fn->type = YL_TYPE_FUNC;
	fn->u.func = malloc(sizeof(fn->u.func));
	CHECK_MEM_ALLOC(fn->u.func);
	if (args->type != AST_LIST) {
		CROAK("Expected a list of arguments to the function!");
	}
	fn->u.func->argc = ast_len(args->val.ast);
	fn->u.func->builtin = 0;
	fn->u.func->u.ast = args->tail;
	fn->u.func->scope = scope;
	fn->u.func->arg_names = malloc(sizeof(char*)*fn->u.func->argc);
	CHECK_MEM_ALLOC(fn->u.func->arg_names);
	ast_get_names(args->val.ast, scope, fn->u.func->arg_names);
	scope_set(scope, identifier, fn);
	return fn;
}

struct YL_Var* not_op(int argc, struct YL_Var** argv)
{
	if (argc == 0) {
		return &YL_TRUE;
	}
	if (argv[0] == &YL_FALSE) {
		return &YL_TRUE;
	} else {
		return &YL_FALSE;
	}
}

int yl_printf(struct YL_Var* var)
{
	int ret = 0;
	switch(var->type) {
	case YL_TYPE_FALSE:
		ret += printf("()");
		break;
	case YL_TYPE_NUMBER:
		{}
		double val = var->u.num;
		if (abs(round(val) - val) < 0.00000001) {
			ret += printf("%d", (int) val);
		} else {
			ret += printf("%f", val);
		}
		break;
	case YL_TYPE_STRING:
		ret += printf("%s", var->u.str);
		break;
	case YL_TYPE_FUNC:
		ret += printf("(def function (");
		for (int i = 0; i < var->u.func->argc; i++) {
			ret += printf("%s ", var->u.func->arg_names[i]);
		}
		ret += printf(") ");
		if (var->u.func->builtin) {
			ret += printf("[native code] ");
		} else {
			ret += ast_printf(var->u.func->u.ast);
		}
		ret += printf(")");
	}
	return ret;
}

struct YL_Var* print_fn(int argc, struct YL_Var** argv)
{
	for(int i = 0; i < argc; i++) {
		yl_printf(argv[i]);
		printf("\n");
	}
	return &YL_FALSE;
}


struct YL_Func DEF_FN = {
	.argc=-1, .builtin=1, .u.builtin_fn=NULL, .arg_names=NULL
};
struct YL_Func PRINT_FN = {
	.argc=1, .builtin=1, .u.builtin_fn=print_fn, .arg_names=NULL
};
struct YL_Func NOT_OP = {
	.argc=1, .builtin=1, .u.builtin_fn=not_op, .arg_names=NULL
};
struct YL_Var BUILTIN_VAR_VALS[] = {
	{ .type=YL_TYPE_FUNC, .u.func=(struct YL_Func*) &DEF_FN },
	{ .type=YL_TYPE_FUNC, .u.func=(struct YL_Func*) &PRINT_FN },
	{ .type=YL_TYPE_FUNC, .u.func=(struct YL_Func*) &NOT_OP }
};
struct YL_VarList NOT_OP_VAR = {
	.name="!", .val=&BUILTIN_VAR_VALS[2], .tail=NULL
};
struct YL_VarList PRINT_FN_VAR = {
	.name="print", .val=&BUILTIN_VAR_VALS[1], .tail=&NOT_OP_VAR
};
struct YL_VarList GLOBAL_VARS = {
	.name="def", .val=&BUILTIN_VAR_VALS[0], .tail=&PRINT_FN_VAR
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
	if (!ast || ast->type == AST_EMPTY)
		return 0;
	else
		return 1 + ast_len(ast->tail);
}

char* cast_yl_var_to_string(struct YL_Var* var)
{
	if (var->type == YL_TYPE_STRING) {
		return var->u.str;
	} else {
		fprintf(stderr, "TODO: Integer cast\n");
		exit(1);
	}
}


struct YL_Var* func_run(struct YL_Func* func, int argc, struct YL_Var** argv)
{
	int i;
	struct YL_Scope* fn_scope = scope_extend(func->scope);
	for (i = 0; i < func->argc; i++) {
		if (i < argc) {
			scope_set(fn_scope, func->arg_names[i], argv[i]);
		} else {
			scope_set(fn_scope, func->arg_names[i], &YL_FALSE);
		}
	}
	return yl_evaluate_in_scope(func->u.ast, fn_scope, 0);
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
			struct AST* fn_name_exp = ast->val.ast;
			char* fn_name = fn_name_exp->val.tok;
			if (strcmp(fn_name, "def") == 0) {
				/* TODO  Call def_fn */
				struct AST* id_exp = ast->val.ast->tail;
				if (!id_exp) {
					CROAK("Missing argument to 'def' function!");
				}
				struct YL_Var* id = yl_evaluate_in_scope(id_exp, scope, 1);
				char* id_s = cast_yl_var_to_string(id);
				return def_fn(id_s, id_exp->tail, scope);
			} else {
				/* Run a normal function */
				struct YL_Var* fn = scope_get(scope, fn_name);
				if (fn->type != YL_TYPE_FUNC) {
					fprintf(stderr, "%s", fn_name);
					CROAK(" is not a function");
				}
				int argc = ast_len(ast->tail);
				struct YL_Var** argv = malloc(sizeof(struct YL_Var*)*argc);
				ast_extract_argv(ast->tail, scope, argv);
				if (fn->u.func->builtin) {
					return fn->u.func->u.builtin_fn(argc, argv);
				} else {
					return func_run(fn->u.func, argc, argv);
				}
			}
		} else {
			struct AST* subexp = ast->val.ast;
			ret = &YL_FALSE;
			while (subexp != NULL) {
				ret = yl_evaluate_in_scope(subexp, scope, 1);
				subexp = ast->tail;
			}
			return ret;
		}
	}
	return &YL_FALSE;
}


struct YL_Var* yl_evaluate(struct AST* ast)
{
	return yl_evaluate_in_scope(ast, &GLOBAL_SCOPE, 1);
}

void yl_print(struct YL_Var* var)
{
	print_fn(1, &var);
}
