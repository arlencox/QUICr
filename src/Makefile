.PHONY: all clean benchmark

PP_FLAGS=$(shell ./check_deps Z3 4.4.0.0 mlbdd 0.5)

FLAGS=-j 0 -cflag -short-paths -pp "cppo $(PP_FLAGS)"

all: _tags quicr/QUICr.mllib
	ocamlbuild -use-ocamlfind $(FLAGS) -I quicr -I main quicr/QUICr.cma quicr/QUICr.cmxa main/Main.d.byte main/Main.native
	rm _tags
	rm quicr/QUICr.mllib

%:%.in
	cppo -n $(PP_FLAGS) -o $@ $<

clean:
	ocamlbuild -clean

install: quicr/META
	ocamlfind install quicr quicr/META $(addprefix _build/quicr/, QUICr.cma QUICr.cmxa QUICr.a QUICr_Internal.cmi QUICr.cmi)
	rm quicr/META

uninstall:
	ocamlfind remove quicr
