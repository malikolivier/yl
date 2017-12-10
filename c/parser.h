#ifndef YL_PARSER
#define YL_PARSER

#include <stdio.h>


enum AST_Type {
	AST_EMPTY,
	AST_VAL,
	AST_LIST
};

struct AST {
	enum AST_Type type;
	union {
		char* tok;
		struct AST* ast;
	} val;
	struct AST* tail;
};

struct AST* yl_parse(FILE* f);
void ast_free(struct AST*);
int ast_printf(struct AST*);

#endif
