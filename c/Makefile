SOURCES=$(wildcard *.c)
OBJECTS=$(patsubst %.c, %.o, $(SOURCES))
EXECUTABLE=yl
WARNINGS=-Wall -Wextra -Wpedantic

.PHONY: clean

$(EXECUTABLE): $(OBJECTS)
	gcc $(OBJECTS) $(WARNINGS) -o $@

$(OBJECTS): %.o : %.c
	gcc $< $(WARNINGS) -c -o $@

clean:
	rm -f $(OBJECTS) $(EXECUTABLE)
