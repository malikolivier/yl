#ifndef YL_INTERPRETER
#define YL_INTERPRETER

enum YL_Type {
	YL_TYPE_FALSE,
	YL_TYPE_NUMBER,
	YL_TYPE_STRING,
	YL_TYPE_FUNC
};

struct YL_Var;

struct YL_Func {
	int argc;
	int builtin;
	union {
		struct YL_Var* (*builtin_fn)(int, struct YL_Var**);
		struct AST* ast;
	} u;
	struct YL_Scope* scope;
	char** arg_names;
};

struct YL_Var {
	enum YL_Type type;
	union {
		double num;
		/* All strings are null-terminated in YL */
		char* str;
		struct YL_Func* func;
	} u;
};

struct YL_VarList {
	int empty;
	char* name;
	struct YL_Var* val;
	struct YL_VarList* tail;
};

struct YL_Scope {
	struct YL_VarList* vars;
	struct YL_Scope* parent;
};


struct YL_Var* yl_evaluate(struct AST*);
struct YL_Var* yl_evaluate_in_scope(struct AST* ast, struct YL_Scope* scope,
                                    int evaluate_function);
void yl_print(struct YL_Var*);

extern struct YL_Scope GLOBAL_SCOPE;

void yl_set_argv(int argc, char** argv);

#endif
