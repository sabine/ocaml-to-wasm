ocamlopt "$1/test.ml" -dcmm &> "$1/test.cmm"
../_build/default/src/ocaml_to_clambda.bc "$1/test.ml"
../_build/default/src/clambda_to_wasm.bc "$1/test.clambda" "$1/test.ir"

