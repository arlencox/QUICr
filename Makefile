.PHONY: all clean

PACKS=$(wildcard *.mlpack)
TARGETS=$(patsubst %.mlpack,%.cma,$(PACKS)) $(patsubst %.mlpack,%.cmxa,$(PACKS))

FLAGS=-cflag -short-paths

all:
	ocamlbuild -use-ocamlfind $(FLAGS) $(TARGETS) Main.d.byte Main.native

clean:
	ocamlbuild -clean

