
TEST=$(wildcard test/*.ml)
ALL=$(wildcard */*.ml)

CMOS=$(ALL:.ml=.cmo)
BYTES=$(TEST:.ml=.byte)
NATIV=$(TEST:.ml=.native)
DEBUG=$(TEST:.ml=.d.byte)


FLAGS=-Is lib,internals,core,various -lib unix -tag thread

.PHONY:byte debug clean cmos

all: cmos tests

cmos:
	ocamlbuild $(FLAGS) $(CMOS)

tests:
	ocamlbuild $(FLAGS) $(BYTES)

debug:
	ocamlbuild $(FLAGS) $(DEBUG)

native:
	ocamlbuild $(FLAGS) $(NATIV)

clean:
	ocamlbuild -clean
