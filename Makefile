
TEST=$(wildcard test/*.ml)

BYTES=$(TEST:.ml=.byte)
DEBUG=$(TEST:.ml=.d.byte)


FLAGS=-Is lib,internals,core -lib unix

.PHONY:byte debug clean

tests:
	ocamlbuild $(FLAGS) $(BYTES)

debug:
	ocamlbuild $(FLAGS) $(DEBUG)

clean:
	ocamlbuild -clean
