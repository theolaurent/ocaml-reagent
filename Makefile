
TEST=$(wildcard test/*.ml)
ALL=$(wildcard */*.ml)

CMOS=$(ALL:.ml=.cmo)
BYTES=$(TEST:.ml=.byte)
DEBUG=$(TEST:.ml=.d.byte)

REAGENTS=ReagentLib.cma

FLAGS=-Is extlib,core,communication,examples -lib unix

.PHONY:byte reagents debug clean cmos

all: cmos reagents tests

reagents:
	ocamlbuild $(FLAGS) $(REAGENTS)

cmos:
	ocamlbuild $(FLAGS) $(CMOS)

tests:
	ocamlbuild $(FLAGS) $(BYTES)

debug:
	ocamlbuild $(FLAGS) $(DEBUG)

clean:
	ocamlbuild -clean
