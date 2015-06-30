
TEST=test_composition.ml test_queue.ml

BYTES=$(TEST:.ml=.byte)
DEBUG=$(TEST:.ml=.d.byte)


FLAGS=-Is lib,internals

.PHONY:byte debug clean

tests:
	ocamlbuild $(FLAGS) $(BYTES)

debug:
	ocamlbuild $(FLAGS) $(DEBUG)

clean:
	ocamlbuild -clean
