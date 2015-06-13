.PHONY: all clean benchmark

FLAGS=-cflag -short-paths

all: Main.ml QUICr.mllib
	ocamlbuild -use-ocamlfind $(FLAGS) Main.d.byte Main.native QUICr.cma QUICr.cmxa

Main.ml _tags QUICr.mllib : Main.ml.in _tags.in QUICr.mllib.in
	./configure

clean:
	ocamlbuild -clean;
	rm Main.ml
	rm _tags

BENCHMARKS.md : Main.native $(wildcard tests/*.strace) $(wildcard tests/*.sdsl)
	python scripts/results > BENCHMARKS.md

benchmark:
	python scripts/results > BENCHMARKS.md

BENCHMARKS.html: BENCHMARKS.md
	pandoc -s BENCHMARKS.md > BENCHMARKS.html

install:
	ocamlfind install quicr META _build/QUICr.cma _build/QUICr.cmxa _build/QUICr.a _build/LogicSymbolicSet.cmi _build/Rename.cmi _build/Access.cmi

uninstall:
	ocamlfind remove quicr
