.PHONY: all clean

OPTIONS=-use-ocamlfind -cflag -annot

all:
	ocamlbuild $(OPTIONS) test.d.byte test.native

clean:
	ocamlbuild -clean
