SOURCES=$(wildcard *.c)
OBJECTS=$(patsubst %.c, %.o, $(SOURCES))
EXECUTABLE=yl
CFLAGS=-Wall -Wextra -Wpedantic
LDFLAGS=-lm

.PHONY: clean

$(EXECUTABLE): $(OBJECTS)
	$(CC) $(OBJECTS) $(CFLAGS) $(LDFLAGS) -o $@

$(OBJECTS): %.o : %.c
	$(CC) -g $< $(CFLAGS) $(LDFLAGS) -c -o $@

clean:
	rm -f $(OBJECTS) $(EXECUTABLE)

tags:
	ctags -R .
