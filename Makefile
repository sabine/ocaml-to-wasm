all:
	dune build lib/clambda_frontend.cma src/ocaml_to_clambda.bc
	dune build lib/clambda_frontend.cma src/clambda_to_wasm.bc

.PHONY: all
