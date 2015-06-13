.PHONY: all clean benchmark

PP_FLAGS=$(shell ./check_deps Z3 4.4.0.0 mlbdd 0.1)

FLAGS=-j 0 -cflag -short-paths -pp "cppo $(PP_FLAGS)"

all: _tags QUICr.mllib
	ocamlbuild -use-ocamlfind $(FLAGS) Main.d.byte Main.native QUICr.cma QUICr.cmxa

%:%.in
	cppo -n $(PP_FLAGS) -o $@ $<

clean:
	ocamlbuild -clean
	-rm _tags
	-rm QUICr.mllib

BENCHMARKS.md : Main.native $(wildcard tests/*.strace) $(wildcard tests/*.sdsl)
	python scripts/results > BENCHMARKS.md

benchmark:
	python scripts/results > BENCHMARKS.md

BENCHMARKS.html: BENCHMARKS.md
	pandoc -s BENCHMARKS.md > BENCHMARKS.html

install: META
	ocamlfind install quicr META _build/QUICr.cma _build/QUICr.cmxa _build/QUICr.a _build/LogicSymbolicSet.cmi _build/Rename.cmi _build/Access.cmi _build/Interface.cmi
	rm META

uninstall:
	ocamlfind remove quicr
