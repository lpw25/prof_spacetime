
CP=cp

OCAMLBUILD_FLAGS=-use-ocamlfind -I src -I bin
OCAMLBUILD=ocamlbuild $(OCAMLBUILD_FLAGS)

all: prof-alloc

FORCE:

main.native: FORCE
	$(OCAMLBUILD) main.native

prof-alloc: main.native
	$(CP) main.native prof-alloc

clean:
	$(OCAMLBUILD) -clean
