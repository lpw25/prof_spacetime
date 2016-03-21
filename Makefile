
OCAMLC=ocamlfind ocamlc
OCAMLOPT=ocamlfind ocamlopt
JS_OF_OCAML=js_of_ocaml --pretty --no-inline
SED=sed

APROF_OCAMLC=~/Repositories/ocaml/_alloc_install/bin/ocamlc
APROF_OCAMLOPT=~/Repositories/ocaml/_alloc_install/bin/ocamlopt

CLIENT_PACKAGES=-package js_of_ocaml -package js_of_ocaml.ppx
CLIENT_OPTS=$(CLIENT_PACKAGES) -I client

SERVER_PACKAGES=-package cmdliner -package cohttp.lwt
SERVER_OPTS=$(SERVER_PACKAGES) -I server

all: prof-alloc

# Client
client/client.cmi: client/client.mli
	$(OCAMLC) $(CLIENT_OPTS) -c $<

client/client.cmo: client/client.ml client/client.cmi
	$(OCAMLC) $(CLIENT_OPTS) -c $<

client/client: client/client.cmo
	$(OCAMLC) $(CLIENT_OPTS)  aprof.cma -linkpkg -o $@ $^

client/client.js: client/client
	$(JS_OF_OCAML) -o $@ $<

# Server
server/embed.ml: server/embed.ml.in client/client.js
	$(SED) -e "/%JS%/r client/client.js" \
	  server/embed.ml.in > server/embed.ml

server/embed.cmi: server/embed.mli
	$(OCAMLC) $(SERVER_OPTS) -c $<

server/embed.cmo: server/embed.ml server/embed.cmi
	$(OCAMLC) $(SERVER_OPTS) -c $<

server/embed.cmx: server/embed.ml server/embed.cmi
	$(OCAMLOPT) $(SERVER_OPTS) -c $<

server/server.cmi: server/server.mli
	$(OCAMLC) $(SERVER_OPTS) -c $<

server/server.cmo: server/server.ml server/server.cmi server/embed.cmi
	$(OCAMLC) $(SERVER_OPTS) -c $<

server/server.cmx: server/server.ml server/server.cmi server/embed.cmi
	$(OCAMLOPT) $(SERVER_OPTS) -c $<

prof-alloc: server/embed.cmx server/server.cmx
	$(OCAMLOPT) $(SERVER_OPTS) aprof.cmxa -linkpkg -o $@ $^
