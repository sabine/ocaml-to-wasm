let usage = "Usage: clambda_to_wasm <options> <files>\nOptions are:"


let read_whole_file filename =
    let ch = open_in filename in
    let s = really_input_string ch (in_channel_length ch) in
    close_in ch;
    s


let main () =
  let ppf_dump = Format.std_formatter in
  let file = Sys.argv.(1) in
  let sexp_string = read_whole_file file in
  let (clambda, _, _) = Clambda_frontend.Clambda_types.clambda_with_constants_of_sexp (Sexplib.Sexp.of_string sexp_string)
  in

  Printclambda.clambda ppf_dump clambda;

  let (ir, fundecls) = Ir.transl clambda in
  let oc = open_out Sys.argv.(2) in
  let ppf = Format.formatter_of_out_channel oc in
  Sexplib0.Sexp.pp_hum_indent 1 ppf (Sexplib.Conv.sexp_of_list Ir.sexp_of_expression ir);
  Printf.fprintf oc "\n\n";
  Sexplib0.Sexp.pp_hum_indent 1 ppf (Sexplib.Conv.sexp_of_list Ir.sexp_of_fundecl fundecls);
  close_out oc

let () =
  main ();
  exit 0
