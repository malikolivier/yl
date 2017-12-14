#ifndef YL_UTIL
#define YL_UTIL

#define CROAK(string) \
	do { \
		fprintf(stderr, "%s\n", string); \
		fprintf(stderr, "Exiting at %s:%d\n", __FILE__, __LINE__); \
		exit(1); \
	} while(0);

#define CHECK_MEM_ALLOC(ptr) \
	do { \
		if (!ptr) { \
			CROAK("Memory allocation failed!"); \
		} \
	} while(0);


#endif
