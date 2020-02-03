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
type    function_symbol = Function of string
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
      * expression * expression list
  | IRprim of primitive * expression list
  | IRconst of constant
  | IRvar of backend_var
  | IRapply of ir_function * expression list
  
  | IRalloc of block

  (*
  | IRUnreachable*)
  | IRNop 
  | IRBlock of block_symbol * expression list
  | IRLoop of block_symbol * expression list
  | IRIf of expression * expression list * expression list
  | IRBr of block_symbol
  | IRBr_if of expression * block_symbol
  | IRBr_if_not of expression * block_symbol
  (*| IRBr_table of block_symbol list * block_symbol*)
  | IRReturn
  | IRCall of ir_function
  | IRCall_indirect of expression * type_symbol
[@@deriving sexp]
and block =
  TODO
and
fundecl =
  { fun_name: function_symbol;
    fun_args: (backend_var_with_provenance * value_kind) list;
    fun_body: expression list;
  }
[@@deriving sexp]
and
constant =
  | IRconst_float of float
  | IRconst_int32 of int32
  | IRconst_int64 of int64
  | IRconst_nativeint of nativeint
  | IRconst_block of int * constant list
  | IRconst_float_array of float list
  | IRconst_string of string

  | IRconst_function of ir_function
  | IRconst_ref of string * constant option
  | IRconst_int of int
[@@deriving sexp]
(*and  
constant = 
  | IRconst_ref of string * structured_constant option
  | IRconst_int of int
  | IRconst_ptr of int
[@@deriving sexp]*)
and
ir_function =
  | Function_symbol of function_symbol
  | Closure of function_symbol * arity * closure_argument list (* invariant: length of argument list is shorter than arity of the closure *)
[@@deriving sexp]
and arity = int
and closure_argument = constant



(* Record application and currying functions *)

let apply_function_sym n =
  Compilenv.need_apply_fun n; Function ("caml_apply" ^ Int.to_string n)
let curry_function_sym n =
  Compilenv.need_curry_fun n;
  if n >= 0
  then Function ("caml_curry" ^ Int.to_string n)
  else Function ("caml_tuplify" ^ Int.to_string (-n))


let rec transl clambda = match clambda with
  | Usequence (instr, clambda') -> 
    let (instr_t, instr_fundecls) = transl instr in
    let (clambda'_t, clambda'_fundecls) = transl clambda' in
    instr_t @ clambda'_t, instr_fundecls @ clambda'_fundecls

  | Uvar (backend_var) -> [IRvar backend_var], []

  | Ulet (mutable_flag, value_kind, backend_var_with_provenance, exp, body) -> 
    let (body_t, body_fundecls) = transl body in
    let (exp_t, exp_fundecls) = transl exp in
    [IRlet (mutable_flag, value_kind, backend_var_with_provenance, List.hd exp_t, body_t)], exp_fundecls @ body_fundecls

  | Uprim (prim, args, _debug) -> 
    let tr = List.map transl args in
    let (args_t, fundecls) = List.split tr in
    [IRprim (prim, List.concat args_t)], List.concat fundecls

  | Uconst (const) -> [IRconst (transl_const const)], []
 
  | Uclosure (ufunction::[], []) ->
    let fundecls_t = transl_fundecl ufunction in
    [IRconst (IRconst_function(Function_symbol (Function ufunction.label)))], fundecls_t

  | Uclosure(fundecls, clos_vars) ->
    transl_closure fundecls clos_vars


  | Uwhile (cond, body) ->
    let block_symbol = new_block_symbol () in
    let (cond_t, cond_fundecls) = transl cond in
    let (body_t, body_fundecls) = transl body in
    [IRLoop (block_symbol, [
            IRBr_if_not (List.hd cond_t, block_symbol)]
            @ body_t
            )], cond_fundecls @ body_fundecls
  
  | _ -> failwith (Format.sprintf "transl not implemented: %s" (Sexplib.Sexp.to_string_hum ~indent:1 (sexp_of_ulambda clambda)))
  
and transl_closure fundecls clos_vars =
  let rec transl_fundecls = function
    [] ->
      let (clos_t, fundecls_t) = List.split (List.map transl clos_vars) in
      clos_t, List.concat fundecls_t
    | f :: rem ->
      let (rem_t, rem_fundecls_t) = transl_fundecls rem in
      let rec to_const ls = match ls with
        | [] -> []
        | x :: xs -> 
            let c = match x with
              | [y] -> (match y with
                | IRconst z -> z
                | _ -> failwith "to_const applied to something that is not a constant")
              | _ -> failwith "to_const applied to something that is not a list with up to one element"
            in
            c :: to_const xs
      in
      if f.arity = 1 || f.arity = 0 then
        [[IRconst (IRconst_function (Closure (Function f.label, f.arity, to_const rem_t)))]], transl_fundecl f @ rem_fundecls_t
      else
      [[IRconst (IRconst_function (Closure (curry_function_sym f.arity, f.arity, IRconst_function (Function_symbol (Function f.label)) :: to_const rem_t)))]], transl_fundecl f @ rem_fundecls_t
  in
  let (clos_t, fundecls_t) = transl_fundecls fundecls in
  List.concat clos_t, fundecls_t

and transl_const uconstant = 
  let transl_structured_const c = match c with 
    | Uconst_float f -> IRconst_float f
    | Uconst_int32 i32 -> IRconst_int32 i32
    | Uconst_int64 i64 -> IRconst_int64 i64
    | Uconst_nativeint i -> IRconst_nativeint i
    | Uconst_block (i, consts) -> IRconst_block (i, List.map transl_const consts)
    | Uconst_float_array float_list -> IRconst_float_array float_list
    | Uconst_string s -> IRconst_string s
    | Uconst_closure (_fundecls, _name, _args) ->
      IRconst_function (Function_symbol (Function "TODO"))
  in
  match uconstant with
    | Uconst_ref (name, const_opt) -> 
      let const_opt_t = match const_opt with
        | None -> None
        | Some c -> Some (transl_structured_const c)
      in 
      IRconst_ref (name, const_opt_t)
    | Uconst_int i -> IRconst_int i
    | Uconst_ptr _ -> failwith "I don't know what to do with ptr"
and transl_fundecl ufunction = 
  let (body_t, body_fundecls) = transl ufunction.body in
  {
    fun_name = Function ufunction.label;
    fun_args = ufunction.params;
    fun_body = body_t;
  } :: body_fundecls


(*
let caml_apply3 arg1 arg2 arg3 closure =
  let Closure (f, arity, args) = closure in
  if arity == 3 then
    [IRapply (Function_symbol f, [arg1; arg2; arg3; closure])]
  else
    let clos2 = Ident.create_local "clos2" in
    let clos3 = Ident.create_local "clos3" in
    (* TODO: find out what to do here, obviously we don't pass the closure value, but instead the identifier we created *)
    [IRLet (Immutable, dfdf, clos2, IRapply (closure, [arg1; closure]));
    IRLet (Immutable, dfdf, clos3, IRapply (clos2, [arg2; clos2]));
    IRapply (clos3, [arg3; clos3])]

*)