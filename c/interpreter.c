#include "parser.h"
#include "interpreter.h"


struct YL_Obj YL_FALSE = { YL_TYPE_FALSE, { (double) 0 } };


struct YL_Obj* yl_evaluate(struct AST* ast)
{
	(void)ast;
	return &YL_FALSE;
}

void yl_print(struct YL_Obj* obj)
{
	(void)obj;
}
