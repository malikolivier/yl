#ifndef YL_INTERPRETER
#define YL_INTERPRETER

#define CROAK(string) \
	do { \
		fprintf(stderr, string); \
		fprintf(stderr, "Exiting at %s:%d", __FILE__, __LINE__); \
		exit(1); \
	} while(0);

enum YL_Type {
	YL_TYPE_FALSE,
	YL_TYPE_NUMBER,
	YL_TYPE_STRING,
	YL_TYPE_FUNC
};

struct YL_Var;

#define DECLARE_YL_Func(len) \
	struct YL_Func_##len { \
		int argc; \
		int builtin; \
		union { \
			struct YL_Var* (*builtin_fn)(int, struct YL_Var*); \
			struct AST* ast; \
		} u; \
		struct YL_Scope* scope; \
		char* arg_names[len]; \
	}

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
DECLARE_YL_Func(2);

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
void yl_print(struct YL_Var*);

#endif
