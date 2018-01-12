SOURCES=$(wildcard *.cc)
HEADERS=$(wildcard *.hh)
OBJECTS=$(patsubst %.cc, %.o, $(SOURCES))
EXECUTABLE=yl
CXXFLAGS=-g -Wall -Wextra -Wpedantic
LDFLAGS=

.PHONY: clean

$(EXECUTABLE): $(OBJECTS) $(HEADERS)
	$(CXX) $(OBJECTS) $(CXXFLAGS) $(LDFLAGS) -o $@

$(OBJECTS): %.o : %.cc $(HEADERS)
	$(CXX) $< $(CXXFLAGS) $(LDFLAGS) -c -o $@

clean:
	rm -f $(OBJECTS) $(EXECUTABLE)

tags:
	ctags -R .
