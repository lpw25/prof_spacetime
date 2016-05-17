
CP=cp

OCAMLBUILD_FLAGS=-use-ocamlfind -I src -I bin -lflags spacetime_lib.cmxa
OCAMLBUILD=ocamlbuild $(OCAMLBUILD_FLAGS)

all: prof-alloc prof-alloc-report

main.native:
	$(OCAMLBUILD) main.native

report.native:
	$(OCAMLBUILD) report.native

prof-alloc: main.native
	$(CP) main.native prof-alloc

prof-alloc-report: report.native
	$(CP) report.native prof-alloc-report

clean:
	$(OCAMLBUILD) -clean
