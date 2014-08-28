.PHONY: all clean

all:
	ocamlbuild -use-ocamlfind test.d.byte test.native

clean:
	ocamlbuild -clean
