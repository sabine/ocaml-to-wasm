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
  | IRlet of mutable_flag * value_kind * backend_var
      * expression * expression list
  | IRprim of primitive * expression list
  | IRconst of constant
  | IRvar of backend_var
  | IRapply of ir_function * expression list
  | IRapply_var of backend_var * expression list
  
  | IRalloc of block

  (*
  | IRUnreachable*)
  | IRNop 
  | IRBlock of block_symbol * expression list
  | IRLoop of block_symbol * expression list
  | IRIf of expression list * expression list * expression list
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
  | IRconst_ptr of int
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
  | Function_closure of closure
  [@@deriving sexp]
and
closure = Closure of function_symbol * arity * closure_argument list   (* invariant: length of argument list is shorter than arity of the closure *)
[@@deriving sexp]
and arity = int
and closure_argument = 
  | ClosureArgConstant of constant
  | ClosureArgVar of backend_var
[@@deriving sexp]


(* TODO: sort out sub expressions, single static assignment form-like stuff *)

(* Record application and currying functions *)

let apply_function_sym n =
  Compilenv.need_apply_fun n; Function ("caml_apply" ^ Int.to_string n)
let curry_function_sym n =
  Compilenv.need_curry_fun n;
  if n >= 0
  then Function ("caml_curry" ^ Int.to_string n)
  else Function ("caml_tuplify" ^ Int.to_string (-n))


let to_closure_arg x = match x with
  | [y] -> (match y with
    | IRconst z -> ClosureArgConstant z
    | IRvar v -> ClosureArgVar v
    | _ -> failwith (Format.sprintf "to_closure_arg applied to: %s" (Sexplib.Sexp.to_string_hum ~indent:1 (sexp_of_expression y))))
  | _ -> failwith "to_closure_arg applied to something that is not a list with up to one element"

let rec transl clambda = match clambda with
  | Usequence (instr, clambda') -> 
    let (instr_t, instr_fundecls) = transl instr in
    let (clambda'_t, clambda'_fundecls) = transl clambda' in
    instr_t @ clambda'_t, instr_fundecls @ clambda'_fundecls

  | Uvar (backend_var) -> [IRvar backend_var], []

  | Ulet (mutable_flag, value_kind, backend_var_with_provenance, exp, body) -> 
    let (body_t, body_fundecls) = transl body in
    let (exp_t, exp_fundecls) = transl exp in
    [IRlet (mutable_flag, value_kind, Backend_var.With_provenance.var backend_var_with_provenance, List.hd exp_t, body_t)], exp_fundecls @ body_fundecls

  | Uprim (prim, args, _debug) -> 
    let tr = List.map transl args in
    let (args_t, fundecls) = List.split tr in
    [IRprim (prim, List.concat args_t)], List.concat fundecls

  | Uconst (const) -> [IRconst (transl_const const)], []

  | Uifthenelse (cond,then_,else_) ->
    let (cond_t, cond_fundecls) = transl cond in
    let (then_t, then_fundecls) = transl then_ in
    let (else_t, else_fundecls) = transl else_ in
    [IRIf (cond_t, then_t, else_t)], cond_fundecls @ then_fundecls @ else_fundecls
 
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
      if f.arity = 1 || f.arity = 0 then
        [[IRconst (IRconst_function (Function_closure (Closure (Function f.label, f.arity, List.map to_closure_arg rem_t))))]], transl_fundecl f @ rem_fundecls_t
      else
      [[IRconst (IRconst_function (Function_closure (
        Closure (curry_function_sym f.arity, f.arity, ClosureArgConstant (IRconst_function (Function_symbol (Function f.label))) :: List.map to_closure_arg rem_t))))]], transl_fundecl f @ rem_fundecls_t
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
    | Uconst_ptr p -> IRconst_ptr p
and transl_fundecl ufunction = 
  let (body_t, body_fundecls) = transl ufunction.body in
  {
    fun_name = Function ufunction.label;
    fun_args = ufunction.params;
    fun_body = body_t;
  } :: body_fundecls



let gen_caml_apply3 arg1 arg2 arg3 closure =
  let Closure (f, arity, args) = closure in
  if arity == 3 then
    [IRapply (Function_symbol f, args @ [arg1; arg2; arg3; to_closure_arg ([IRconst (IRconst_function (Function_closure closure))])])]
  else
    let clos2 = Ident.create_local "clos2" in
    let clos3 = Ident.create_local "clos3" in
    (* TODO: find out what to do here, obviously we don't pass the closure value, but instead the identifier we created *)
    [IRlet (Immutable, Pgenval, clos2, IRapply (Function_closure closure, [arg1; to_closure_arg ([IRconst (IRconst_function (Function_closure closure))])]);
      [IRlet (Immutable, Pgenval, clos3, IRapply_var (clos2, [arg2; IRvar clos2]);
      [IRapply_var (clos3, [arg3; IRvar clos3])])])]


let gen_caml_curry3 arg closure =
  let Closure (_f, arity, _args) = closure in
  if arity == 3 then
    [Closure (Function "caml_curry3_1", 2, to_closure_arg [arg] :: [to_closure_arg [IRconst (IRconst_function (Function_closure closure))]])]
  else
    failwith "Arity of closure is not 3"

