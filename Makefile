#
# Makefile
#

SRC= def.ml parser.mly lexer.mll eval.ml main.ml
COMPONENT= def.ml parser.mli parser.ml lexer.ml eval.ml main.ml
TARGET= testaml

all:	$(TARGET)

$(TARGET): 	$(COMPONENT) 
	ocamlc $(COMPONENT) -o $(TARGET)

parser.mli:	parser.mly def.mli
	ocamlyacc parser.mly

parser.ml:	parser.mly def.mli
	ocamlyacc parser.mly

lexer.ml:	lexer.mll
	ocamllex lexer.mll

def.mli:
	ocamlc def.ml
backup:
	/bin/cp -f Makefile $(SRC) back

clean:
	/bin/rm -f parser.ml parser.mli lexer.ml $(TARGET) *.cmi *.cmo *.mli

