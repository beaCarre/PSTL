CLASSPATH=/usr/lib/jvm/java-7-oracle/lib/ct.sym
LIB=javalib.cmja
OPT=-cp $(CLASSPATH)
JE=-java-extensions
WRAP=-java-package wraptest

SRC=testPoint


default: clean compile run

clean:
	rm -f *.cm* *.jo *~ $(SRC).jar

compile:
	ocamljava $(OPT) $(LIB) $(JE) $(WRAP) -c $(SRC).ml
	ocamljava $(OPT) $(LIB) $(JE) $(WRAP) -c $(SRC).mli
	ocamljava $(OPT) $(LIB) $(JE) $(WRAP) -o $(SRC).jar $(SRC).cmj

wrap: 
	ocamlwrap $(SRC).cmi
	javac -cp $(SRC).jar Celsuis_fahrenheitWrapper.java

run :
	java -jar $(SRC).jar

