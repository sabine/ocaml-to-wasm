(* Entrypoint of our own backend *)

let interface ~source_file ~output_prefix =
  Optcompile.interface ~source_file ~output_prefix

let implementation ~backend ~source_file ~output_prefix =
  Optcompile.implementation ~backend ~source_file ~output_prefix
