#include <stdio.h>
#include <stdlib.h>

#include "parser.h"


struct InputStream {
	int pos;
	int line;
	int col;
};

char input_stream_next(struct InputStream* stream)
{
	(void)stream;
	return EOF;
}

void ast_free(struct AST* ast)
{
	if (ast != NULL) {
		free(ast->tok);
		ast_free(ast->tail);
		free(ast);
	}
}

struct AST* yl_parse(FILE* f)
{
	struct AST* ast = NULL;
	if (ast == NULL) {
		ast = malloc(sizeof(struct AST*));
		(void)f;
	}
	return ast;
}

