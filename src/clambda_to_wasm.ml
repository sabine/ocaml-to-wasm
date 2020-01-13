let usage = "Usage: clambda_to_wasm <options> <files>\nOptions are:"

let main () =
  let ppf_dump = Format.std_formatter in
  let file = Sys.argv.(1) in
  let ic = open_in file in
  let sexp_string = input_line ic in
  let _ = Sexplib0.Sexp_conv.triple_of_sexp
    (Clambda_frontend.Clambda_types.ulambda_of_sexp)
    (Sexplib0.Sexp_conv.list_of_sexp Clambda_frontend.Clambda_types.preallocated_block_of_sexp)
    (Sexplib0.Sexp_conv.list_of_sexp Clambda_frontend.Clambda_types.preallocated_constant_of_sexp)
    (Sexplib.Sexp.of_string sexp_string)
  in

  Sexplib0.Sexp.pp_hum_indent 2 ppf_dump (Sexplib.Sexp.of_string sexp_string);
  close_in ic

let () =
  main ();
  exit 0
