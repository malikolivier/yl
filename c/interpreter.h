#ifndef YL_INTERPRETER
#define YL_INTERPRETER

enum YL_Type {
	YL_TYPE_FALSE,
	YL_TYPE_NUMBER,
	YL_TYPE_STRING
};

struct YL_Obj {
	enum YL_Type type;
	union {
		double num;
		struct {
			char* val;
			int len;
		} str;
	} value;
};

struct YL_Obj* yl_evaluate(struct AST*);

#endif
