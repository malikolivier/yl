#ifndef YL_PARSER
#define YL_PARSER

#include <stdio.h>


struct AST {
	char* tok;
	struct AST* tail;
};

struct AST* yl_parse(FILE* f);
void ast_free(struct AST*);

#endif
