#include <stdio.h>
#include <stdlib.h>

#include "parser.h"

#define BUF_SIZE 64

enum YL_TokenType {
	YL_TOKEN_PUNC,
	YL_TOKEN_SYM
};

struct YL_Token {
	enum YL_TokenType type;
	char* val;
};

struct YL_InputStream {
	int line;
	int col;
	FILE* f;
};

struct YL_TokenStream {
	struct YL_Token* cur;
	struct YL_InputStream* input;
};

char inputstream_peek(struct YL_InputStream* stream)
{
	char ch = fgetc(stream->f);
	fseek(stream->f, -1L, SEEK_CUR);
	return ch;
}

char inputstream_next(struct YL_InputStream* stream)
{
	char ch = fgetc(stream->f);
	if (ch == '\n') {
		stream->line++;
		stream->col = 0;
	} else {
		stream->col++;
	}
	return ch;
}

void inputstream_croak(struct YL_InputStream* stream, char* msg)
{
	fprintf(stderr, "%s (%d:%d)", msg, stream->line, stream->col);
	exit(1);
}

int token_eof(struct YL_TokenStream* tokstream)
{
	return feof(tokstream->input->f);
}

char* token_read_while(struct YL_TokenStream* tokstream,
                       int (*predicate)(char))
{
	int size = 0;
	int pos = 0;
	char* buf = NULL;
	while(!token_eof(tokstream) &&
              predicate(inputstream_peek(tokstream->input))) {
		char c = inputstream_next(tokstream->input);
		if (pos >= size - 1) {
			// Increase buffer size
			size += BUF_SIZE;
			buf = realloc(buf, size);
			if (!buf) {
				fprintf(stderr, "Memory allocation failed\n");
				exit(1);
			}
		}
		buf[pos] = c;
		pos++;
	}
	if (buf)	buf[pos] = '\0';
	return buf;
}

char* token_read_escaped(struct YL_TokenStream* tokstream, char end)
{
	int escaped = 0;
	char* buf = NULL;
	int size = 0;
	int pos = 0;
	char add_char = 0;
	while(!token_eof(tokstream)) {
		char ch = inputstream_next(tokstream->input);
		if (escaped) {
			add_char = 1;
			escaped = 0;
		} else if (ch == '\\') {
			add_char = 0;
			escaped = 1;
		} else if (ch == end) {
			ch = '\0';
			add_char = 1;
		} else {
			add_char = 1;
		}
		if (add_char) {
			if (pos >= size) {
				// Increase buffer size
				size += BUF_SIZE;
				buf = realloc(buf, size);
				if (!buf) {
					fprintf(stderr, "Memory allocation failed\n");
					exit(1);
				}
			}
			buf[pos] = ch;
			pos++;
		}
	}
	return buf;
}

int is_whitespace(char ch)
{
	return ch == ' ' || ch == '\t' || ch == '\n';
}

int is_parenthesis(char ch) {
	return ch == '(' || ch == ')';
}

int is_symbol(char ch)
{
	return !is_parenthesis(ch) && !is_whitespace(ch);
}

int is_not_newline(char ch)
{
	return ch != '\n';
}

void skip_comment(struct YL_TokenStream* tokstream)
{
	token_read_while(tokstream, is_not_newline);
	inputstream_next(tokstream->input);
}


struct YL_Token* token_read_symbol(struct YL_TokenStream* tokstream)
{
	char* identifier = token_read_while(tokstream, is_symbol);
	struct YL_Token* tok = malloc(sizeof(tok));
	tok->type = YL_TOKEN_SYM;
	tok->val = identifier;
	return tok;
}

struct YL_Token* token_read_string(struct YL_TokenStream* tokstream)
{
	char* identifier = token_read_escaped(tokstream, '"');
	struct YL_Token* tok = malloc(sizeof(tok));
	tok->type = YL_TOKEN_SYM;
	tok->val = identifier;
	return tok;
}

struct YL_Token* token_read_next(struct YL_TokenStream* tokstream)
{
	token_read_while(tokstream, is_whitespace);
	if (token_eof(tokstream))
		return NULL;
	char ch = inputstream_peek(tokstream->input);
	if (ch == ';') {
		skip_comment(tokstream);
		return token_read_next(tokstream);
	}
	if (ch == '"') {
		return token_read_string(tokstream);
	}
	if (is_symbol(ch)) {
		return token_read_symbol(tokstream);
	}
	if (is_parenthesis(ch)) {
		struct YL_Token* tok = malloc(sizeof(tok));
		tok->type = YL_TOKEN_PUNC;
		tok->val = malloc(2);
		if (!tok->val) {
				fprintf(stderr, "Memory allocation failed\n");
				exit(1);
		}
		*(tok->val) = ch;
		*(tok->val + 1) = '\0';
		inputstream_next(tokstream->input);
		return tok;
	}
	char* error_msg = malloc(256);
	sprintf(error_msg, "Can't handle %c", ch);
	inputstream_croak(tokstream->input, error_msg);
	free(error_msg);
	return NULL;
}

struct YL_Token* token_next(struct YL_TokenStream* tokstream)
{
	struct YL_Token* tok = tokstream->cur;
	tokstream->cur = NULL;
	if (tok)	return tok;
	else		return token_read_next(tokstream);
}

int token_is_closing(struct YL_Token* tok)
{
	return tok->type == YL_TOKEN_PUNC && tok->val[0] == ')';
}
int token_is_opening(struct YL_Token* tok)
{
	return tok->type == YL_TOKEN_PUNC && tok->val[0] == '(';
}

void ast_free(struct AST* ast)
{
	if (ast->type == AST_EMPTY) {
		free(ast);
	} else if (ast->type == AST_VAL) {
		free(ast->val.tok);
		if (ast->tail)
			ast_free(ast->tail);
	} else if (ast->type == AST_LIST) {
		ast_free(ast->val.ast);
		if (ast->tail)
			ast_free(ast->tail);
	}
}


struct AST* parse_tok_stream(struct YL_TokenStream* tokstream)
{
	struct AST* ast = malloc(sizeof(ast));
	ast->type = AST_EMPTY;
	struct AST* next_ast = ast;
	struct YL_Token* tok = NULL;
	int second_loop = 0;
	while ((tok = token_next(tokstream)) && !token_is_closing(tok)) {
		if (second_loop) {
			next_ast->tail = malloc(sizeof(ast));
			next_ast = next_ast->tail;
			next_ast->type = AST_EMPTY;
		}
		if (tok->type == YL_TOKEN_SYM) {
			next_ast->val.tok = tok->val;
		} else if (token_is_opening(tok)) {
			next_ast->val.ast = parse_tok_stream(tokstream);
		}
		next_ast->tail = NULL;
		second_loop = 1;
	}
	return ast;
}

struct AST* yl_parse(FILE* f)
{
	struct YL_InputStream stream = { 0, 0, f };
	struct YL_TokenStream tokstream = { NULL, &stream };
	return parse_tok_stream(&tokstream);
}

int ast_fprintf(FILE* f, struct AST* ast)
{
	int ret = 0;
	ret += fprintf(f, "(");
	if (ast->type == AST_VAL) {
		ret += fprintf(f, "\"%s\" ", ast->val.tok);
	} else if (ast->type == AST_LIST) {
		ret += ast_fprintf(f, ast->val.ast);
	}
	ret += fprintf(f, ")");
	return ret;
}

int ast_printf(struct AST* ast)
{
	return ast_fprintf(stdin, ast);
}
