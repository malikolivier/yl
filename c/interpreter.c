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

struct YL_Var* yl_var_new_number(double n)
{
	struct YL_Var* var = malloc(sizeof(var));
	CHECK_MEM_ALLOC(var);
	var->type = YL_TYPE_NUMBER;
	var->u.num = n;
	return var;
}

struct YL_Var* yl_var_new_string(char* s)
{
	struct YL_Var* var = malloc(sizeof(var));
	CHECK_MEM_ALLOC(var);
	var->type = YL_TYPE_STRING;
	var->u.str = s;
	return var;
}


struct YL_VarList* varlist_prepend(struct YL_VarList* varlist,
                                   char* id, struct YL_Var* val)
{
	struct YL_VarList* new_var;
	if (varlist->empty) {
		new_var = varlist;
		new_var->tail = NULL;
	} else {
		new_var = malloc(sizeof(new_var));
		CHECK_MEM_ALLOC(new_var);
		new_var->tail = varlist;
	}
	new_var->empty = 0;
	new_var->name = id;
	new_var->val = val;
	return new_var;
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
	scope->vars = varlist_prepend(scope->vars, id, val);
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
		ast = ast->tail;
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

struct YL_Var* let_fn(int argc, struct YL_Var** argv, struct YL_Scope* scope) {
	if (argc <= 0)
		return &YL_FALSE;
	char* id = cast_yl_var_to_string(argv[0]);
	struct YL_Var* rhs = argc > 1 ? argv[1] : &YL_FALSE;
	return scope_set(scope, id, rhs);
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

int dfloat_equal(double d1, double d2)
{
	return fabs(d1 - d2) < 0.00000001;
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
		if (dfloat_equal(round(val), val)) {
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
		if (!var->u.func->arg_names) {
			ret += printf("...");
			goto end_of_arguments_output;
		}
		for (int i = 0; i < var->u.func->argc; i++) {
			ret += printf("%s ", var->u.func->arg_names[i]);
		}
end_of_arguments_output:
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

struct YL_Var* eq_op(int argc, struct YL_Var** argv)
{
	(void)argc; /* Assumes 2 arguments were given */
	if (argv[0]->type != argv[1]->type)
		return &YL_FALSE;
	switch(argv[0]->type) {
	case YL_TYPE_FALSE:
		if (argv[1]->type == YL_TYPE_FALSE)
			return &YL_TRUE;
		else
			return &YL_FALSE;
	case YL_TYPE_NUMBER:
		if (dfloat_equal(argv[0]->u.num, argv[1]->u.num))
			return &YL_TRUE;
		else
			return &YL_FALSE;
	case YL_TYPE_STRING:
		if (strcmp(argv[0]->u.str, argv[1]->u.str) == 0)
			return &YL_TRUE;
		else
			return &YL_FALSE;
	case YL_TYPE_FUNC:
		if (argv[0]->u.func == argv[1]->u.func)
			return &YL_TRUE;
		else
			return &YL_FALSE;
	default:
		CROAK("Why I am here?");
		return &YL_FALSE;
	}
}

struct YL_Var* lt_op(int argc, struct YL_Var** argv)
{
	(void)argc; /*  Assumes 2 arguments were given */
	if (argv[0]->type != argv[1]->type)
		return &YL_FALSE;
	switch(argv[0]->type) {
	case YL_TYPE_FALSE:
		/* YL_FALSE is lesser than everything */
		if (argv[1]->type != YL_TYPE_FALSE)
			return &YL_TRUE;
		else
			return &YL_FALSE;
	case YL_TYPE_NUMBER:
		if (argv[0]->u.num < argv[1]->u.num)
			return &YL_TRUE;
		else
			return &YL_FALSE;
	case YL_TYPE_STRING:
		if (strcmp(argv[0]->u.str, argv[1]->u.str) < 0)
			return &YL_TRUE;
		else
			return &YL_FALSE;
	case YL_TYPE_FUNC:
		return &YL_FALSE;
	default:
		CROAK("Why I am here?");
		return &YL_FALSE;
	}
}

struct YL_Var* le_op(int argc, struct YL_Var** argv) {
	return (lt_op(argc, argv) == &YL_TRUE) ||
               (eq_op(argc, argv) == &YL_TRUE) ? &YL_TRUE : &YL_FALSE;
}

struct YL_Var* gt_op(int argc, struct YL_Var** argv) {
	return (le_op(argc, argv) == &YL_TRUE) ? &YL_FALSE : &YL_TRUE;
}

struct YL_Var* ge_op(int argc, struct YL_Var** argv) {
	return (lt_op(argc, argv) == &YL_TRUE) ? &YL_FALSE : &YL_TRUE;
}

struct YL_Var* plus_op(int argc, struct YL_Var** argv) {
	int i;
	int any_non_number = 0;
	struct YL_Var* ret;
	for (i = 0; i < argc; i++) {
		if (argv[i]->type != YL_TYPE_NUMBER) {
			any_non_number = 1;
			break;
		}
	}

	if (any_non_number) {
		/* TODO Cast all non-strings to strings */
		int total_len = 1;
		for (i = 0; i < argc; i++) {
			total_len += strlen(argv[i]->u.str);
		}
		char* concat_str = malloc(total_len);
		CHECK_MEM_ALLOC(concat_str);
		int j = 0;
		for (i = 0; i < argc; i++) {
			char c = argv[i]->u.str[j];
			while (c != '\0')
				concat_str[j++] = c;
		}
		concat_str[j++] = '\0';
		ret = yl_var_new_string(concat_str);
	} else {
		double out = 0;
		for (i = 0; i < argc; i++) {
			out += argv[i]->u.num;
		}
		ret = yl_var_new_number(out);
	}
	return ret;
}

struct YL_Var* minus_op(int argc, struct YL_Var** argv) {
	/* !BEWARE!  Undefined behaviour if arguments are not numbers! */
	(void)argc;
	return yl_var_new_number(argv[0]->u.num - argv[1]->u.num);
}

struct YL_Var* multiply_op(int argc, struct YL_Var** argv) {
	/* !BEWARE!  Undefined behaviour if arguments are not numbers! */
	(void)argc;
	return yl_var_new_number(argv[0]->u.num * argv[1]->u.num);
}

struct YL_Var* divide_op(int argc, struct YL_Var** argv) {
	/* !BEWARE!  Undefined behaviour if arguments are not numbers! */
	(void)argc;
	return yl_var_new_number(argv[0]->u.num / argv[1]->u.num);
}

struct YL_Var* modulo_op(int argc, struct YL_Var** argv) {
	/* !BEWARE!  Undefined behaviour if arguments are not numbers! */
	(void)argc;
	double n = argv[0]->u.num;
	double mod = abs(argv[1]->u.num);
	double ret = n;
	while (abs(ret) >= mod || ret < 0) {
		if (n < 0)
			ret += mod;
		else
			ret -= mod;
	}
	return yl_var_new_number(ret);
}

struct YL_Func DEF_FN = {
	.argc=-1, .builtin=1, .u.builtin_fn=NULL, .arg_names=NULL
};
struct YL_Func LET_FN = {
	.argc=-1, .builtin=1, .u.builtin_fn=NULL, .arg_names=NULL
};
struct YL_Func PRINT_FN = {
	.argc=1, .builtin=1, .u.builtin_fn=print_fn, .arg_names=NULL
};
struct YL_Func NOT_OP = {
	.argc=1, .builtin=1, .u.builtin_fn=not_op, .arg_names=NULL
};
struct YL_Func EQ_OP = {
	.argc=2, .builtin=1, .u.builtin_fn=eq_op, .arg_names=NULL
};
struct YL_Func LT_OP = {
	.argc=2, .builtin=1, .u.builtin_fn=lt_op, .arg_names=NULL
};
struct YL_Func LE_OP = {
	.argc=2, .builtin=1, .u.builtin_fn=le_op, .arg_names=NULL
};
struct YL_Func GT_OP = {
	.argc=2, .builtin=1, .u.builtin_fn=gt_op, .arg_names=NULL
};
struct YL_Func GE_OP = {
	.argc=2, .builtin=1, .u.builtin_fn=ge_op, .arg_names=NULL
};
struct YL_Func PLUS_OP = {
	.argc=2, .builtin=1, .u.builtin_fn=plus_op, .arg_names=NULL
};
struct YL_Func MINUS_OP = {
	.argc=2, .builtin=1, .u.builtin_fn=minus_op, .arg_names=NULL
};
struct YL_Func MULTIPLY_OP = {
	.argc=2, .builtin=1, .u.builtin_fn=multiply_op, .arg_names=NULL
};
struct YL_Func DIVIDE_OP = {
	.argc=2, .builtin=1, .u.builtin_fn=divide_op, .arg_names=NULL
};
struct YL_Func MODULO_OP = {
	.argc=2, .builtin=1, .u.builtin_fn=modulo_op, .arg_names=NULL
};
struct YL_Var BUILTIN_VAR_VALS[] = {
	{ .type=YL_TYPE_FUNC, .u.func=(struct YL_Func*) &DEF_FN },
	{ .type=YL_TYPE_FUNC, .u.func=(struct YL_Func*) &LET_FN },
	{ .type=YL_TYPE_FUNC, .u.func=(struct YL_Func*) &PRINT_FN },
	{ .type=YL_TYPE_FUNC, .u.func=(struct YL_Func*) &NOT_OP },
	{ .type=YL_TYPE_FUNC, .u.func=(struct YL_Func*) &EQ_OP },
	{ .type=YL_TYPE_FUNC, .u.func=(struct YL_Func*) &LT_OP },
	{ .type=YL_TYPE_FUNC, .u.func=(struct YL_Func*) &LE_OP },
	{ .type=YL_TYPE_FUNC, .u.func=(struct YL_Func*) &GT_OP },
	{ .type=YL_TYPE_FUNC, .u.func=(struct YL_Func*) &GE_OP },
	{ .type=YL_TYPE_FUNC, .u.func=(struct YL_Func*) &PLUS_OP },
	{ .type=YL_TYPE_FUNC, .u.func=(struct YL_Func*) &MINUS_OP },
	{ .type=YL_TYPE_FUNC, .u.func=(struct YL_Func*) &DIVIDE_OP },
	{ .type=YL_TYPE_FUNC, .u.func=(struct YL_Func*) &MODULO_OP }
};
struct YL_VarList MODULO_OP_VAR = {
	.name="%", .val=&BUILTIN_VAR_VALS[13], .tail=NULL
};
struct YL_VarList DIVIDE_OP_VAR = {
	.name="/", .val=&BUILTIN_VAR_VALS[12], .tail=&MODULO_OP_VAR
};
struct YL_VarList MULTIPLY_OP_VAR = {
	.name="*", .val=&BUILTIN_VAR_VALS[11], .tail=&DIVIDE_OP_VAR
};
struct YL_VarList MINUS_OP_VAR = {
	.name="-", .val=&BUILTIN_VAR_VALS[10], .tail=&MULTIPLY_OP_VAR
};
struct YL_VarList PLUS_OP_VAR = {
	.name="+", .val=&BUILTIN_VAR_VALS[9], .tail=&MINUS_OP_VAR
};
struct YL_VarList GE_OP_VAR = {
	.name=">=", .val=&BUILTIN_VAR_VALS[8], .tail=&PLUS_OP_VAR
};
struct YL_VarList GT_OP_VAR = {
	.name=">", .val=&BUILTIN_VAR_VALS[7], .tail=&GE_OP_VAR
};
struct YL_VarList LE_OP_VAR = {
	.name="<=", .val=&BUILTIN_VAR_VALS[6], .tail=&GT_OP_VAR
};
struct YL_VarList LT_OP_VAR = {
	.name="<", .val=&BUILTIN_VAR_VALS[5], .tail=&LE_OP_VAR
};
struct YL_VarList EQ_OP_VAR = {
	.name="=", .val=&BUILTIN_VAR_VALS[4], .tail=&LT_OP_VAR
};
struct YL_VarList NOT_OP_VAR = {
	.name="!", .val=&BUILTIN_VAR_VALS[3], .tail=&EQ_OP_VAR
};
struct YL_VarList PRINT_FN_VAR = {
	.name="print", .val=&BUILTIN_VAR_VALS[2], .tail=&NOT_OP_VAR
};
struct YL_VarList LET_FN_VAR = {
	.name="let", .val=&BUILTIN_VAR_VALS[1], .tail=&PRINT_FN_VAR
};
struct YL_VarList GLOBAL_VARS = {
	.name="def", .val=&BUILTIN_VAR_VALS[0], .tail=&LET_FN_VAR
};
struct YL_Scope GLOBAL_SCOPE = { .vars=&GLOBAL_VARS, .parent=NULL };

int is_number(char* str)
{
	char* endptr;
	double out = strtod(str, &endptr);
	if (out == 0 && str == endptr) {
		return 0;
	} else {
		return *endptr == '\0';
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
		CROAK("TODO: Integer cast\n");
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
			double num = strtod(ast->val.tok, NULL);
			return yl_var_new_number(num);
		} else {
			return yl_var_new_string(ast->val.tok);
		}
	case AST_LIST:
		if (evaluate_function && ast_len(ast->val.ast) > 0 &&
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
				int argc = ast_len(fn_name_exp->tail);
				struct YL_Var** argv = malloc(sizeof(struct YL_Var*)*argc);
				ast_extract_argv(fn_name_exp->tail, scope, argv);
				if (fn->u.func->builtin) {
					if (strcmp(fn_name, "let") == 0) {
						return let_fn(argc, argv, scope);
					}
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
				subexp = subexp->tail;
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
