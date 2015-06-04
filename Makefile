.PHONY: all clean benchmark

PACKS=$(wildcard *.mlpack)
TARGETS=$(patsubst %.mlpack,%.cma,$(PACKS)) $(patsubst %.mlpack,%.cmxa,$(PACKS))

FLAGS=-cflag -short-paths

all: Main.ml
	ocamlbuild -use-ocamlfind $(FLAGS) $(TARGETS) Main.d.byte Main.native

Main.ml : Main.ml.in
	./configure

clean:
	ocamlbuild -clean;
	rm Main.ml

BENCHMARKS.md : Main.native $(wildcard tests/*.strace) $(wildcard tests/*.sdsl)
	python scripts/results > BENCHMARKS.md

benchmark:
	python scripts/results > BENCHMARKS.md

BENCHMARKS.html: BENCHMARKS.md
	pandoc -s BENCHMARKS.md > BENCHMARKS.html
