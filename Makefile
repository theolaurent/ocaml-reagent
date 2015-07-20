
TEST=$(wildcard test/*.ml)

BYTES=$(TEST:.ml=.byte)
DEBUG=$(TEST:.ml=.d.byte)

REAGENTS=ReagentLib.cma

FLAGS=-Is extlib,core,communication,examples -lib unix

.PHONY: tests reagents debug clean

all: reagents tests

reagents:
	ocamlbuild $(FLAGS) $(REAGENTS)

tests:
	ocamlbuild $(FLAGS) $(BYTES)

debug:
	ocamlbuild $(FLAGS) $(DEBUG)

clean:
	ocamlbuild -clean
