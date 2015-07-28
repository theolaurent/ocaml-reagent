
TEST=test/composition.byte test/sum_queue.byte test/queue_test.byte test/swap.byte test/trivial.byte test/queue_test_low.byte test/stack_test.byte

EXS=$(wildcard examples/*.ml)

EXCMOS=$(EXS:.ml=.cmo)
DEBUG=$(TEST:.byte=.d.byte)

REAGENTS=ReagentLib.cma

FLAGS=-Is extlib,core,communication,examples,test -lib unix

.PHONY: tests reagents debug clean examples

all: reagents examples tests

reagents:
	ocamlbuild $(FLAGS) $(REAGENTS)

tests:
	ocamlbuild $(FLAGS) $(TEST)

examples:
	ocamlbuild $(FLAGS) $(EXCMOS)


debug:
	ocamlbuild $(FLAGS) $(DEBUG)

clean:
	ocamlbuild -clean
