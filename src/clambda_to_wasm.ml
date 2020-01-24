let usage = "Usage: clambda_to_wasm <options> <files>\nOptions are:"

let main () =
  let ppf_dump = Format.std_formatter in
  let file = Sys.argv.(1) in
  let ic = open_in file in
  let sexp_string = input_line ic in
  let (clambda, _, _) = Clambda_frontend.Clambda_types.clambda_with_constants_of_sexp (Sexplib.Sexp.of_string sexp_string)
  in
  close_in ic;
  (*Sexplib0.Sexp.pp_hum_indent 1 ppf_dump (Sexplib.Sexp.of_string sexp_string);*)

  Printclambda.clambda ppf_dump clambda;

  let (ir, fundecls) = Ir.transl clambda in
  let oc = open_out Sys.argv.(2) in
  let ppf = Format.formatter_of_out_channel oc in
  Sexplib0.Sexp.pp_hum_indent 1 ppf (Ir.sexp_of_expression ir);
  Printf.fprintf oc "\n\n";
  Sexplib0.Sexp.pp_hum_indent 1 ppf (Sexplib.Conv.sexp_of_list Ir.sexp_of_fundecl fundecls);
  close_out oc

let () =
  main ();
  exit 0
