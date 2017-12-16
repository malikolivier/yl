#include <stdio.h>
#include <string.h>
#include <errno.h>

#include "parser.h"
#include "interpreter.h"

#define PROMPT_MAXLEN 65536


static void usage(void)
{
	puts("yl [-e \"Inline code\"] [file]");
}


int main(int argc, char** argv)
{
	int ret;
	FILE* f;
	struct AST* ast;
	struct YL_Var* obj;
	if (argc < 2) {
		/* Interactive CLI */
		char code[PROMPT_MAXLEN];
		int col = 0;
		printf("> ");
		while (1) {
			int c = getchar();
			if (c == EOF) {
				break;
			} else if (c == '\n') {
				f = fmemopen(code, col, "r");
				if (!f) {
					printf("Failed to open memory! %s", strerror(errno));
					return 1;
				}
				col = 0;
				ast = yl_parse(f);
				fclose(f);
				/* ast_printf(ast);
				   puts(""); */
				obj = yl_evaluate_in_scope(ast, &GLOBAL_SCOPE, 0);
				yl_print(obj);
				ast_free(ast);
				printf("> ");
			} else {
				if (col > PROMPT_MAXLEN) {
					printf("Input too long! ( > %d)", PROMPT_MAXLEN);
					return 1;
				}
				code[col++] = c;
			}
		}
		return 0;
	}
	if (strncmp(argv[1], "-e", 2) == 0) {
		if (argc < 2) {
			usage();
			return 1;
		}
		f = fmemopen(argv[2], strlen(argv[2]), "r");
		if (!f) {
			fprintf(stderr, "Could not open memory stream\n");
			return 1;
		}
		yl_set_argv(argc - 3, argv + 3);
	} else {
		f = fopen(argv[1], "r");
		if (!f) {
			fprintf(stderr, "Failed to open file '%s'\n", argv[1]);
			usage();
			return 1;
		}
		yl_set_argv(argc - 2, argv + 2);
	}
	ast = yl_parse(f);
	fclose(f);
	obj = yl_evaluate(ast);
	if (obj->type != YL_TYPE_NUMBER)
		ret = 0;
	else {
		ret = (int) obj->u.num;
	}
	ast_free(ast);
	return ret;
}
