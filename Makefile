
CP=cp

OCAMLBUILD_FLAGS=-use-ocamlfind -I src -I bin
OCAMLBUILD=ocamlbuild $(OCAMLBUILD_FLAGS)

all: prof_spacetime

FORCE:

main.native: FORCE
	$(OCAMLBUILD) main.native

prof_spacetime: main.native
	$(CP) main.native prof_spacetime

clean:
	$(OCAMLBUILD) -clean
