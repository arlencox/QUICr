.PHONY: all clean benchmark

PACKS=$(wildcard *.mlpack)
TARGETS=$(patsubst %.mlpack,%.cma,$(PACKS)) $(patsubst %.mlpack,%.cmxa,$(PACKS))

FLAGS=-cflag -short-paths

all:
	ocamlbuild -use-ocamlfind $(FLAGS) $(TARGETS) Main.d.byte Main.native

Main.native: all

clean:
	ocamlbuild -clean

BENCHMARKS.md : Main.native $(wildcard tests/*.strace) $(wildcard tests/*.sdsl)
	python scripts/results > BENCHMARKS.md

benchmark:
	python scripts/results > BENCHMARKS.md

BENCHMARKS.html: BENCHMARKS.md
	pandoc -s BENCHMARKS.md > BENCHMARKS.html
