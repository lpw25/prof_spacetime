
CP=cp

OCAMLBUILD_FLAGS=-use-ocamlfind -I src -I bin -lflags spacetime_lib.cmxa
OCAMLBUILD=ocamlbuild $(OCAMLBUILD_FLAGS)

all: prof-alloc

main.native:
	$(OCAMLBUILD) main.native

prof-alloc: main.native
	$(CP) main.native prof-alloc

clean:
	$(OCAMLBUILD) -clean
