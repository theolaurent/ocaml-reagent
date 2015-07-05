
TEST=$(wildcard test/*.ml)
ALL=$(wildcard */*.ml)

CMOS=$(ALL:.ml=.cmo)
BYTES=$(TEST:.ml=.byte)
DEBUG=$(TEST:.ml=.d.byte)


FLAGS=-Is lib,internals,core,various -lib unix

.PHONY:byte debug clean cmos

all: cmos tests

cmos:
	ocamlbuild $(FLAGS) $(CMOS)

tests:
	ocamlbuild $(FLAGS) $(BYTES)

debug:
	ocamlbuild $(FLAGS) $(DEBUG)

clean:
	ocamlbuild -clean
