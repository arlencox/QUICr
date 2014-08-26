.PHONY: all clean

all:
	ocamlbuild -use-ocamlfind test.d.byte

clean:
	ocamlbuild -clean
