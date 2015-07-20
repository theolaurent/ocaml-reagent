
TEST=$(wildcard test/*.ml)

BYTES=$(TEST:.ml=.byte)
NATIV=$(TEST:.ml=.native)
DEBUG=$(TEST:.ml=.d.byte)

REAGENTS=ReagentLib.cma

FLAGS=-Is extlib,core,communication,examples -lib unix -tag thread

.PHONY: tests reagents debug clean

all: reagents tests

reagents:
	ocamlbuild $(FLAGS) $(REAGENTS)

tests:
	ocamlbuild $(FLAGS) $(BYTES)

debug:
	ocamlbuild $(FLAGS) $(DEBUG)

native:
	ocamlbuild $(FLAGS) $(NATIV)

clean:
	ocamlbuild -clean
