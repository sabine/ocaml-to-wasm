open Clambda_frontend.Clambda_types

open Sexplib0
open Sexp_conv

(* 
type ulambda =
    Uvar of backend_var
  | Uconst of uconstant
  | Udirect_apply of function_label * ulambda list * debuginfo
  | Ugeneric_apply of ulambda * ulambda list * debuginfo
  | Uclosure of ufunction list * ulambda list
  | Uoffset of ulambda * int
  | Ulet of mutable_flag * value_kind * backend_var_with_provenance
      * ulambda * ulambda
  | Uphantom_let of backend_var_with_provenance
      * uphantom_defining_expr option * ulambda
  | Uletrec of (backend_var_with_provenance * ulambda) list * ulambda
  | Uprim of primitive * ulambda list * debuginfo
  | Uswitch of ulambda * ulambda_switch * debuginfo
  | Ustringswitch of ulambda * (string * ulambda) list * ulambda option
  | Ustaticfail of int * ulambda list
  | Ucatch of
      int *
      (backend_var_with_provenance * value_kind) list *
      ulambda *
      ulambda
  | Utrywith of ulambda * backend_var_with_provenance * ulambda
  | Uifthenelse of ulambda * ulambda * ulambda
  | Usequence of ulambda * ulambda
  | Uwhile of ulambda * ulambda
  | Ufor of backend_var_with_provenance * ulambda * ulambda
      * direction_flag * ulambda
  | Uassign of backend_var * ulambda
  | Usend of meth_kind * ulambda * ulambda * ulambda list * debuginfo
  | Uunreachable
  TODO: change this completely so that it makes sense with WASM control flow *)


type   block_symbol = Block of int
[@@deriving sexp]
type function_symbol = Function of string
[@@deriving sexp]
type    local_symbol = Local of string
[@@deriving sexp]
type   global_symbol = Global of string
[@@deriving sexp]
type     type_symbol = Type of string
[@@deriving sexp]

type block_type = value_kind
[@@deriving sexp]


let new_block_symbol = 
  let last = ref 0 in 
  fun () -> incr last ; Block !last


type expression = 
  | IRlet of mutable_flag * value_kind * backend_var_with_provenance
      * expression * expression
  | IRprim of primitive * expression list
  | IRconst of uconstant
  | IRvar of backend_var
  | IRfunction_symbol of function_symbol
  
  (*
  | IRUnreachable*)
  | IRNop 
  | IRSequence of expression * expression
  | IRBlock of block_symbol * expression
  | IRLoop of block_symbol * expression
  | IRIf of expression * expression * expression 
  | IRBr of block_symbol
  | IRBr_if of expression * block_symbol
  | IRBr_if_not of expression * block_symbol
  (*| IRBr_table of block_symbol list * block_symbol*)
  | IRReturn
  | IRCall of function_symbol
  | IRCall_indirect of expression * type_symbol
[@@deriving sexp]
and
fundecl =
  { fun_name: function_symbol;
    fun_args: (backend_var_with_provenance * value_kind) list;
    fun_body: expression;
  }
[@@deriving sexp]


let rec transl clambda = match clambda with
  | Usequence (instr, clambda') -> 
    let (instr_t, instr_fundecls) = transl instr in
    let (clambda'_t, clambda'_fundecls) = transl clambda' in
    IRSequence (instr_t, clambda'_t), instr_fundecls @ clambda'_fundecls

  | Uvar (backend_var) -> IRvar backend_var, []

  | Ulet (mutable_flag, value_kind, backend_var_with_provenance, exp, body) -> 
    let (body_t, body_fundecls) = transl body in
    let (exp_t, exp_fundecls) = transl exp in
    IRlet (mutable_flag, value_kind, backend_var_with_provenance, exp_t, body_t), exp_fundecls @ body_fundecls

  | Uprim (prim, args, _debug) -> 
    let tr = List.map transl args in
    let (args_t, fundecls) = List.split tr in
    IRprim (prim, args_t), List.concat fundecls

  | Uconst (const) -> IRconst const, []
 
  | Uclosure (ufunction::[], _env) ->
    let (body_t, body_fundecls) = transl ufunction.body in
    IRfunction_symbol (Function ufunction.label), [{
      fun_name = Function ufunction.label;
      fun_args = ufunction.params;
      fun_body = body_t;
    }] @ body_fundecls

  | Uwhile (cond, body) ->
    let block_symbol = new_block_symbol () in
    let (cond_t, cond_fundecls) = transl cond in
    let (body_t, body_fundecls) = transl body in
    IRLoop (block_symbol, IRSequence (
            IRBr_if_not (cond_t, block_symbol),
            body_t
            )), cond_fundecls @ body_fundecls
  
  | _ -> failwith (Format.sprintf "transl not implemented: %s" (Sexplib.Sexp.to_string_hum ~indent:1 (sexp_of_ulambda clambda)))
