SOURCES=$(wildcard *.cc)
OBJECTS=$(patsubst %.cc, %.o, $(SOURCES))
EXECUTABLE=yl
CXXFLAGS=-g -Wall -Wextra -Wpedantic
LDFLAGS=

.PHONY: clean

$(EXECUTABLE): $(OBJECTS)
	$(CXX) $(OBJECTS) $(CXXFLAGS) $(LDFLAGS) -o $@

$(OBJECTS): %.o : %.cc
	$(CXX) $< $(CXXFLAGS) $(LDFLAGS) -c -o $@

clean:
	rm -f $(OBJECTS) $(EXECUTABLE)

tags:
	ctags -R .
