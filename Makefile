.PHONY: all clean benchmark

PACKS=$(wildcard *.mlpack)
TARGETS=$(patsubst %.mlpack,%.cma,$(PACKS)) $(patsubst %.mlpack,%.cmxa,$(PACKS))

FLAGS=-cflag -short-paths

all:
	ocamlbuild -use-ocamlfind $(FLAGS) $(TARGETS) Main.d.byte Main.native

clean:
	ocamlbuild -clean
	-rm BENCHMARKS.md

benchmark:
	python results > BENCHMARKS.md
