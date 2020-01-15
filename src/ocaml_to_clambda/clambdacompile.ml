(* NOTE: this is all very ugly, partly due to using the version of the compiler that I did. When we upgrade to a more recent OCaml compiler version, it is best to redo this completely, since these parts of the OCaml compiler have been refactored in a more recent version. *)

(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* asmcomp/asmgen.ml changed to dump Clambda sexp *)

(* From lambda to assembly code *)

[@@@ocaml.warning "+a-4-9-40-41-42"]

open Format
open Config
open Clflags
open Misc

type error = Assembler_error of string

exception Error of error

let liveness phrase = Liveness.fundecl phrase; phrase

let dump_if ppf flag message phrase =
  if !flag then Printmach.phase message ppf phrase

let pass_dump_if ppf flag message phrase =
  dump_if ppf flag message phrase; phrase

let pass_dump_linear_if ppf flag message phrase =
  if !flag then fprintf ppf "*** %s@.%a@." message Printlinear.fundecl phrase;
  phrase

let flambda_raw_clambda_dump_if ppf
      ({ Flambda_to_clambda. expr = ulambda; preallocated_blocks = _;
        structured_constants; exported = _; } as input) =
  if !dump_rawclambda then
    begin
      Format.fprintf ppf "@.clambda (before Un_anf):@.";
      Printclambda.clambda ppf ulambda;
      Symbol.Map.iter (fun sym cst ->
          Format.fprintf ppf "%a:@ %a@."
            Symbol.print sym
            Printclambda.structured_constant cst)
        structured_constants
    end;
  if !dump_cmm then Format.fprintf ppf "@.cmm:@.";
  input

type clambda_and_constants =
  Clambda.ulambda *
  Clambda.preallocated_block list *
  Clambda.preallocated_constant list

let raw_clambda_dump_if ppf
      ((ulambda, _, structured_constants):clambda_and_constants) =
  if !dump_rawclambda || !dump_clambda then
    begin
      Format.fprintf ppf "@.clambda:@.";
      Printclambda.clambda ppf ulambda;
      List.iter (fun {Clambda.symbol; definition} ->
          Format.fprintf ppf "%s:@ %a@."
            symbol
            Printclambda.structured_constant definition)
        structured_constants
    end;
  if !dump_cmm then Format.fprintf ppf "@.cmm:@."

let rec regalloc ~ppf_dump round fd =
  if round > 50 then
    fatal_error(fd.Mach.fun_name ^
                ": function too complex, cannot complete register allocation");
  dump_if ppf_dump dump_live "Liveness analysis" fd;
  if !use_linscan then begin
    (* Linear Scan *)
    Interval.build_intervals fd;
    if !dump_interval then Printmach.intervals ppf_dump ();
    Linscan.allocate_registers()
  end else begin
    (* Graph Coloring *)
    Interf.build_graph fd;
    if !dump_interf then Printmach.interferences ppf_dump ();
    if !dump_prefer then Printmach.preferences ppf_dump ();
    Coloring.allocate_registers()
  end;
  dump_if ppf_dump dump_regalloc "After register allocation" fd;
  let (newfd, redo_regalloc) = Reload.fundecl fd in
  dump_if ppf_dump dump_reload "After insertion of reloading code" newfd;
  if redo_regalloc then begin
    Reg.reinit(); Liveness.fundecl newfd; regalloc ~ppf_dump (round + 1) newfd
  end else newfd

let (++) x f = f x

let compile_fundecl ~ppf_dump fd_cmm =
  Proc.init ();
  Cmmgen.reset ();
  Reg.reset();
  fd_cmm
  ++ Profile.record ~accumulate:true "selection" Selection.fundecl
  ++ pass_dump_if ppf_dump dump_selection "After instruction selection"
  ++ Profile.record ~accumulate:true "comballoc" Comballoc.fundecl
  ++ pass_dump_if ppf_dump dump_combine "After allocation combining"
  ++ Profile.record ~accumulate:true "cse" CSE.fundecl
  ++ pass_dump_if ppf_dump dump_cse "After CSE"
  ++ Profile.record ~accumulate:true "liveness" liveness
  ++ Profile.record ~accumulate:true "deadcode" Deadcode.fundecl
  ++ pass_dump_if ppf_dump dump_live "Liveness analysis"
  ++ Profile.record ~accumulate:true "spill" Spill.fundecl
  ++ Profile.record ~accumulate:true "liveness" liveness
  ++ pass_dump_if ppf_dump dump_spill "After spilling"
  ++ Profile.record ~accumulate:true "split" Split.fundecl
  ++ pass_dump_if ppf_dump dump_split "After live range splitting"
  ++ Profile.record ~accumulate:true "liveness" liveness
  ++ Profile.record ~accumulate:true "regalloc" (regalloc ~ppf_dump 1)
  ++ Profile.record ~accumulate:true "available_regs" Available_regs.fundecl
  ++ Profile.record ~accumulate:true "linearize" Linearize.fundecl
  ++ pass_dump_linear_if ppf_dump dump_linear "Linearized code"
  ++ Profile.record ~accumulate:true "scheduling" Scheduling.fundecl
  ++ pass_dump_linear_if ppf_dump dump_scheduling "After instruction scheduling"
  ++ Profile.record ~accumulate:true "emit" Emit.fundecl

let set_export_info (ulambda, prealloc, structured_constants, export) =
  Compilenv.set_export_info export;
  (ulambda, prealloc, structured_constants)

let end_gen_implementation ?toplevel prefixname
    (clambda:clambda_and_constants) =
  let ch = open_out (prefixname ^ ".clambda") in
  let ppf = Format.formatter_of_out_channel ch in
  let _ = toplevel in
  let sexp_string = Sexplib0.Sexp.to_string (Clambda_frontend.Clambda_types.sexp_of_clambda_with_constants clambda)
  in
  (* instead of emitting Assembly, we just dump the Clambda sexps *)
    Format.fprintf ppf "%s" sexp_string;
    close_out ch

let flambda_gen_implementation ?toplevel prefixname ~backend ~ppf_dump
    (program:Flambda.program) =
  let export = Build_export_info.build_transient ~backend program in
  let (clambda, preallocated, constants) =
    Profile.record_call "backend" (fun () ->
      (program, export)
      ++ Flambda_to_clambda.convert
      ++ flambda_raw_clambda_dump_if ppf_dump
      ++ (fun { Flambda_to_clambda. expr; preallocated_blocks;
                structured_constants; exported; } ->
             (* "init_code" following the name used in
                [Cmmgen.compunit_and_constants]. *)
           Un_anf.apply ~ppf_dump expr ~what:"init_code", preallocated_blocks,
           structured_constants, exported)
      ++ set_export_info)
  in
  let constants =
    List.map (fun (symbol, definition) ->
        { Clambda.symbol = Linkage_name.to_string (Symbol.label symbol);
          exported = true;
          definition;
          provenance = None;
        })
      (Symbol.Map.bindings constants)
  in
  end_gen_implementation ?toplevel prefixname
    (clambda, preallocated, constants)

let lambda_gen_implementation ?toplevel prefixname ~backend ~ppf_dump
    (lambda:Lambda.program) =
  let clambda =
    Closure.intro ~backend ~size:lambda.main_module_block_size lambda.code
  in
  let provenance : Clambda.usymbol_provenance =
    { original_idents = [];
      module_path =
        Path.Pident (Ident.create_persistent (Compilenv.current_unit_name ()));
    }
  in
  let preallocated_block =
    Clambda.{
      symbol = Compilenv.make_symbol None;
      exported = true;
      tag = 0;
      fields = List.init lambda.main_module_block_size (fun _ -> None);
      provenance = Some provenance;
    }
  in
  let clambda_and_constants =
    clambda, [preallocated_block], Compilenv.structured_constants ()
  in
  Compilenv.clear_structured_constants ();
  raw_clambda_dump_if ppf_dump clambda_and_constants;
  end_gen_implementation ?toplevel prefixname clambda_and_constants

let compile_implementation_gen ?toplevel prefixname
    ~required_globals ~ppf_dump gen_implementation program =
  let asmfile =
    if !keep_asm_file || !Emitaux.binary_backend_available
    then prefixname ^ ext_asm
    else Filename.temp_file "camlasm" ext_asm
  in
  Asmgen.compile_unit prefixname asmfile !keep_asm_file
      (prefixname ^ ext_obj) (fun () ->
        Ident.Set.iter Compilenv.require_global required_globals;
        gen_implementation ?toplevel prefixname ~ppf_dump program)

let compile_implementation_clambda ?toplevel prefixname
    ~backend ~ppf_dump (program:Lambda.program) =
  compile_implementation_gen ?toplevel prefixname
    ~required_globals:program.Lambda.required_globals
    ~ppf_dump (lambda_gen_implementation ~backend) program

let compile_implementation_flambda ?toplevel prefixname
    ~required_globals ~backend ~ppf_dump (program:Flambda.program) =
  compile_implementation_gen ?toplevel prefixname
    ~required_globals ~ppf_dump (flambda_gen_implementation ~backend) program

(* Error report *)

let report_error ppf = function
  | Assembler_error file ->
      fprintf ppf "Assembler error, input left in file %a"
        Location.print_filename file

let () =
  Location.register_error_of_exn
    (function
      | Error err -> Some (Location.error_of_printer_file report_error err)
      | _ -> None
    )



(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 2002 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* This corresponds to driver/optcompile.ml, changed to compile only to Clambda sexp *)

(** The batch compiler *)

open Misc
open Compile_common

let tool_name = "ocaml2clambda"

let with_info =
  Compile_common.with_info ~native:true ~tool_name

let interface ~source_file ~output_prefix =
  with_info ~source_file ~output_prefix ~dump_ext:"cmi" @@ fun info ->
  Compile_common.interface info

let (|>>) (x, y) f = (x, f y)

(** Native compilation backend for .ml files. *)

let flambda i backend typed =
  if !Clflags.classic_inlining then begin
    Clflags.default_simplify_rounds := 1;
    Clflags.use_inlining_arguments_set Clflags.classic_arguments;
    Clflags.unbox_free_vars_of_closures := false;
    Clflags.unbox_specialised_args := false
  end;
  typed
  |> Profile.(record transl)
      (Translmod.transl_implementation_flambda i.module_name)
  |> Profile.(record generate)
    (fun {Lambda.module_ident; main_module_block_size;
          required_globals; code } ->
    ((module_ident, main_module_block_size), code)
    |>> print_if i.ppf_dump Clflags.dump_rawlambda Printlambda.lambda
    |>> Simplif.simplify_lambda
    |>> print_if i.ppf_dump Clflags.dump_lambda Printlambda.lambda
    |> (fun ((module_ident, size), lam) ->
      Flambda_middle_end.middle_end
        ~ppf_dump:i.ppf_dump
        ~prefixname:i.output_prefix
        ~size
        ~filename:i.source_file
        ~module_ident
        ~backend
        ~module_initializer:lam)
    |> compile_implementation_flambda
      i.output_prefix ~required_globals ~backend ~ppf_dump:i.ppf_dump;
    Compilenv.save_unit_info (cmx i))

let clambda i backend typed =
  Clflags.use_inlining_arguments_set Clflags.classic_arguments;
  typed
  |> Profile.(record transl)
    (Translmod.transl_store_implementation i.module_name)
  |> print_if i.ppf_dump Clflags.dump_rawlambda Printlambda.program
  |> Profile.(record generate)
    (fun program ->
       let code = Simplif.simplify_lambda program.Lambda.code in
       { program with Lambda.code }
       |> print_if i.ppf_dump Clflags.dump_lambda Printlambda.program
       |> compile_implementation_clambda
         i.output_prefix ~backend ~ppf_dump:i.ppf_dump;
       Compilenv.save_unit_info (cmx i))

let implementation ~backend ~source_file ~output_prefix =
  let backend info typed =
    Compilenv.reset ?packname:!Clflags.for_package info.module_name;
    if Config.flambda
    then flambda info backend typed
    else clambda info backend typed
  in
  with_info ~source_file ~output_prefix ~dump_ext:"cmx" @@ fun info ->
  Compile_common.implementation info ~backend

