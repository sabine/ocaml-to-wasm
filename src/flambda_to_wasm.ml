let usage = "Usage: flambda_to_wasm <options> <files>\nOptions are:"


let read_whole_file filename =
    let ch = open_in filename in
    let s = really_input_string ch (in_channel_length ch) in
    close_in ch;
    s


let main () =
  let file = Sys.argv.(1) in
  let sexp_string = read_whole_file file in
  let _ppf_dump = Format.std_formatter in
  let _flambda_unit = Flambda_frontend.Flambda_types.flambda_unit_of_sexp (Sexplib.Sexp.of_string sexp_string)
  in
  ()

let () =
  main ();
  exit 0
