
TEST=test/composition.byte test/sum_queue.byte test/queue_test.byte test/swap.byte test/trivial.byte

DEBUG=$(TEST:.byte=.d.byte)

REAGENTS=ReagentLib.cma

FLAGS=-Is extlib,core,communication,examples,test -lib unix

.PHONY: tests reagents debug clean

all: reagents tests

reagents:
	ocamlbuild $(FLAGS) $(REAGENTS)

tests:
	ocamlbuild $(FLAGS) $(TEST)

debug:
	ocamlbuild $(FLAGS) $(DEBUG)

clean:
	ocamlbuild -clean
