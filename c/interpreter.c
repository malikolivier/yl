#include "parser.h"
#include "interpreter.h"


struct YL_Obj YL_FALSE = { YL_TYPE_FALSE, 0 };


struct YL_Obj* yl_evaluate(struct AST* ast)
{
	return &YL_FALSE;
}
