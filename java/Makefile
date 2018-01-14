JFLAGS = -g
JC = javac
.SUFFIXES: .java .class
.PHONY: clean run

.java.class:
	$(JC) $(JFLAGS) $*.java

CLASSES = $(wildcard com/boussejra/yl/*.java) \
          $(wildcard com/boussejra/yl/**/*.java)

classes: $(CLASSES:.java=.class)

run: $(CLASSES:.java=.class)
	java com.boussejra.yl.Main

clean:
	$(RM) $(CLASSES:.java=.class)
