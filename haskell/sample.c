/* Necessary headers */
#include <stdlib.h>
#include <assert.h> /* For assert */
#include <stdarg.h> /* For valist, in print and add */
#include <string.h> /* For string concatenation, etc. */

/* Conditional headers */
#include <stdio.h> /* for 'print' function */

/* declare struct */
/* Used for objects whose type is unknown at compile time */
enum var_type {
	VAR_TYPE_FALSE,
	VAR_TYPE_INT,
	VAR_TYPE_FLOAT,
	VAR_TYPE_STRING,
	VAR_TYPE_FUNC
};

struct var {
	enum var_type type;
	union {
		long i;
		double f;
		char* str;
		struct func* fn;
	};
};

struct func {
	void (*fn)(int num, ...);
	struct var *captured_vars;
	int captured_var_count;
};

/* Global variables */
int ARGC;
struct var* ARGV;
struct var RET = { .type=VAR_TYPE_FALSE };


/* Helper functions */

void error(char* err)
{
	fprintf(stderr, "%s\nAborting...\n", err);
	exit(1);
}

struct var var_from_string(char* str)
{
	if (*str == '\0') {
		struct var ret = { .type=VAR_TYPE_STRING, .str=""};
		return ret;
	}
	char* endptr;
	long i = strtol(str, &endptr, 10);
	if (*endptr == '\0') {
		struct var ret = { .type=VAR_TYPE_INT, .i=i};
		return ret;
	}
	double f = strtod(str, &endptr);
	if (*endptr == '\0') {
		struct var ret = { .type=VAR_TYPE_FLOAT, .f=f};
		return ret;
	}
	struct var ret = { .type=VAR_TYPE_STRING, .str=str};
	return ret;
}


void init_args(int argc, char** argv)
{
	ARGC = argc - 1;
	ARGV = malloc(sizeof(struct var) * ARGC);
	if (!ARGV) {
		error("Unable to allocate memory.");
	}
	for (int i = 0; i < ARGC; i++) {
		ARGV[i] = var_from_string(argv[i + 1]);
	}
}

void print_one(struct var obj)
{
	switch(obj.type) {
	case VAR_TYPE_FALSE:
		printf("()\n");
		break;
	case VAR_TYPE_INT:
		printf("%ld\n", obj.i);
		break;
	case VAR_TYPE_FLOAT:
		printf("%f\n", obj.f);
		break;
	case VAR_TYPE_STRING:
		printf("%s\n", obj.str);
		break;
	case VAR_TYPE_FUNC:
		printf("function object at %p\n", obj.fn);
		break;
	default:
		error("Unknown type!");
	}
}

void argv(struct var obj)
{
	int i = obj.i;
	if (i < ARGC) {
		RET = ARGV[i];
	} else {
		RET.type = VAR_TYPE_FALSE;
	}
}

int var_toi(struct var obj)
{
	switch(obj.type) {
	case VAR_TYPE_INT:
		return obj.i;
	case VAR_TYPE_FLOAT:
		return obj.f;
	default:
		return 0;
	}
}

char* concat(char *s1, char *s2)
{
    char *ret = malloc(strlen(s1) + strlen(s2) + 1);
    assert(ret != NULL);
    strcpy(ret, s1);
    strcat(ret, s2);
    return ret;
}

char* _ltoa(long i)
{
	/* We can hopefully assume i has less than 16 digits */
	char *ret = malloc(16);
	snprintf(ret, 16, "%ld", i);
	return ret;
}

char* _dtoa(double d)
{
	/* We can hopefully assume d has less than 16 digits */
	char *ret = malloc(16);
	snprintf(ret, 16, "%f", d);
	return ret;
}

/* Addition helper functions */
struct var binary_add(struct var obj1, struct var obj2)
{
	struct var ret = { .type=VAR_TYPE_FALSE };
	switch(obj1.type) {
	case VAR_TYPE_INT:
		if (obj2.type == VAR_TYPE_INT) {
			ret.type = VAR_TYPE_INT;
			ret.i = obj1.i + obj2.i;
		} else if (obj2.type == VAR_TYPE_FLOAT) {
			ret.type = VAR_TYPE_FLOAT;
			ret.f = obj1.i + obj2.f;
		} else if (obj2.type == VAR_TYPE_STRING) {
			ret.type = VAR_TYPE_STRING;
			ret.str = concat(_ltoa(obj1.i), obj2.str);
		}
		break;
	case VAR_TYPE_FLOAT:
		if (obj2.type == VAR_TYPE_INT) {
			ret.type = VAR_TYPE_FLOAT;
			ret.i = obj1.f + obj2.i;
		} else if (obj2.type == VAR_TYPE_FLOAT) {
			ret.type = VAR_TYPE_FLOAT;
			ret.f = obj1.f + obj2.f;
		} else if (obj2.type == VAR_TYPE_STRING) {
			ret.type = VAR_TYPE_STRING;
			ret.str = concat(_dtoa(obj1.f), obj2.str);
		}
		break;
	case VAR_TYPE_STRING:
		if (obj2.type == VAR_TYPE_INT) {
			ret.type = VAR_TYPE_STRING;
			ret.str = concat(obj1.str, _ltoa(obj2.i));
		} else if (obj2.type == VAR_TYPE_FLOAT) {
			ret.type = VAR_TYPE_STRING;
			ret.str = concat(obj1.str, _dtoa(obj2.f));
		} else if (obj2.type == VAR_TYPE_STRING) {
			ret.type = VAR_TYPE_STRING;
			ret.str = concat(obj1.str, obj2.str);
		}
		break;
	}
	return ret;
}

void add(int num, ...)
{
	/* Assume num > 0 */
	va_list valist;
	va_start(valist, num);
	RET = va_arg(valist, struct var);
	for (int i = 1; i < num; i++) {
		RET = binary_add(RET, va_arg(valist, struct var));
	}
	va_end(valist);
}

void print(int num, ...)
{
	va_list valist;
	va_start(valist, num);
	for (int i = 0; i < num; i++) {
		print_one(va_arg(valist, struct var));
	}
	va_end(valist);
	RET.type = VAR_TYPE_FALSE;
}

/* Example of generated user defined function */
/* (def my_function (a b) (+ a b))            */
void my_function_0_fn(int num, ...)
{
	/* First set parameters */
	va_list valist;
	va_start(valist, num);
	struct var a_0;
	struct var b_0;
	if (num > 0)
		a_0 = va_arg(valist, struct var);
	else
		a_0.type = VAR_TYPE_FALSE;
	if (num > 1)
		b_0 = va_arg(valist, struct var);
	else
		b_0.type = VAR_TYPE_FALSE;
	/* ... Go on until all parameters are set */
	va_end(valist);
	add(2, a_0, b_0);
}

/* Example of generated user defined function */
/* (let x 1)
   (def add_to_x (a) (+ x a))
   (add_to_x x)                               */
/* Function should be a global variable       */
struct var add_to_x_0;
void add_to_x_0_fn(int num, ...);

void top_level_scope()
{
	RET.type = VAR_TYPE_INT;
	RET.i = 1;
	struct var x_0 = RET;
	/* Define function */
	add_to_x_0.type = VAR_TYPE_FUNC;
	add_to_x_0.fn = malloc(sizeof(struct func));
	assert(add_to_x_0.fn != NULL);
	add_to_x_0.fn->fn = add_to_x_0_fn;
	struct var *captured_vars = malloc(sizeof(struct var) * 1); /* captured_var_count = 1 */
	assert(captured_vars != NULL);
	captured_vars[0] = x_0;
	add_to_x_0.fn->captured_vars = captured_vars;
	add_to_x_0.fn->captured_var_count = 1;
	RET = add_to_x_0;
	/* End define function */
	add_to_x_0.fn->fn(1, x_0);
}

void add_to_x_0_fn(int num, ...)
{
	/* First set parameters */
	va_list valist;
	va_start(valist, num);
	struct var a_0;
	if (num > 0)
		a_0 = va_arg(valist, struct var);
	else
		a_0.type = VAR_TYPE_FALSE;
	/* ... Go on until all parameters are set */
	va_end(valist);
	/* Loop on captured values */
	struct var x_0 = add_to_x_0.fn->captured_vars[0];
	/* ... Go on setting captured values */
	add(2, x_0, a_0);
}



int main(int argc, char** argv_)
{
	init_args(argc, argv_);
	RET.type = VAR_TYPE_INT;
	RET.i = 0;
	argv(RET);
	print(1, RET);
	return var_toi(RET);
}