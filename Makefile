
CP=cp

OCAMLBUILD_FLAGS=-use-ocamlfind -I server -lflags aprof.cmxa
OCAMLBUILD=ocamlbuild $(OCAMLBUILD_FLAGS)

all: prof-alloc

# Server
server.native:
	$(OCAMLBUILD) server.native

prof-alloc: server.native
	$(CP) server.native prof-alloc

clean:
	$(OCAMLBUILD) -clean
