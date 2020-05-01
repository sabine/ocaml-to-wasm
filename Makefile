all:
	dune build lib/clambda_frontend.cma src/ocaml_to_clambda.bc
	dune build lib/clambda_frontend.cma src/clambda_to_wasm.bc
	dune build lib/flambda_frontend.cma src/flambda_to_wasm.bc
	dune build lib/flambda_frontend.cma src/flambda_interpreter.bc


.PHONY: all
