
CP=cp

OCAMLBUILD_FLAGS=-use-ocamlfind -I src -I bin
OCAMLBUILD=ocamlbuild $(OCAMLBUILD_FLAGS)

all: prof-spacetime

FORCE:

main.native: FORCE
	$(OCAMLBUILD) main.native

prof-spacetime: main.native
	$(CP) main.native prof-spacetime

clean:
	$(OCAMLBUILD) -clean
