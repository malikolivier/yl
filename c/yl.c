#include <stdio.h>
#include <string.h>

#include "parser.h"
#include "interpreter.h"


static void usage(void)
{
	puts("yl [-e \"Inline code\"] [file]");
}


int main(int argc, char** argv)
{
	int ret;
	FILE* f;
	if (argc < 2) {
		/* Interactive CLI */
		while (1) {
			/* TODO */
			puts("Not implemented!");
			return 1;
		}
	}
	if (argc < 3) {
		f = fopen(argv[1], "r");
		if (!f) {
			fprintf(stderr, "Failed to open file '%s'\n", argv[1]);
			usage();
			return 1;
		}
	} else {
		if (strncmp(argv[1], "-e", 2) == 0) {
			f = fmemopen(argv[2], strlen(argv[2]), "r");
			if (!f) {
				fprintf(stderr, "Could not open memory stream\n");
				return 1;
			}
		} else {
			usage();
			return 1;
		}
	}
	struct AST* ast = yl_parse(f);
	struct YL_Obj* obj = yl_evaluate(ast);
	ast_free(ast);
	if (obj->type != YL_TYPE_NUMBER)
		ret = 0;
	else {
		ret = (int) obj->value.num;
	}
	return ret;
}
