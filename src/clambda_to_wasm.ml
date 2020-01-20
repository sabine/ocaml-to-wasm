let usage = "Usage: clambda_to_wasm <options> <files>\nOptions are:"

let main () =
  let ppf_dump = Format.std_formatter in
  let file = Sys.argv.(1) in
  let ic = open_in file in
  let sexp_string = input_line ic in
  let (clambda, _, _) = Clambda_frontend.Clambda_types.clambda_with_constants_of_sexp (Sexplib.Sexp.of_string sexp_string)
  in
  close_in ic;
  Sexplib0.Sexp.pp_hum_indent 2 ppf_dump (Sexplib.Sexp.of_string sexp_string);

  Format.fprintf ppf_dump "\nclambda_to_wasm understands this as:\n\n";
  Printclambda.clambda ppf_dump clambda;

  let instruction_list = Ir.clambda_to_instruction_list clambda []
  in
  Format.fprintf ppf_dump "\nIR: %s\n" (Sexplib.Sexp.to_string (Sexplib0.Sexp_conv.sexp_of_list Ir.sexp_of_instruction instruction_list))

let () =
  main ();
  exit 0
