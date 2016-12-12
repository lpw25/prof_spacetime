
CP=cp
RM=rm

OCAMLBUILD_FLAGS=-use-ocamlfind -I src -I bin
OCAMLBUILD=ocamlbuild $(OCAMLBUILD_FLAGS)

BINARY=prof_spacetime

all: $(BINARY)

FORCE:

main.native: FORCE
	$(OCAMLBUILD) main.native

$(BINARY): main.native
	$(CP) main.native $@

clean:
	$(OCAMLBUILD) -clean
	$(RM) -f $(BINARY)
