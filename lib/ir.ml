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


type   block_symbol = Block of string
[@@deriving sexp]
type function_symbol = Function of string
[@@deriving sexp]
type    local_symbol = Local of string
[@@deriving sexp]
type   global_symbol = Global of string
[@@deriving sexp]
type     type_symbol = Type of string
[@@deriving sexp]

type value_type = I32_type | I64_type | F32_type | F64_type
[@@deriving sexp]
type block_type = value_type
[@@deriving sexp]

type instruction =
  | SetLocal of local_symbol * ulambda
  | GetLocal of local_symbol
  | SetGlobal of global_symbol * ulambda
  | GetGlobal of global_symbol

  | Unreachable
  | Nop
  | Block of block_symbol * block_type * instruction list
  | Loop of block_type * instruction list
  | If of block_type * instruction list * instruction list 
  | Br of block_symbol
  | Br_if of block_symbol
  | Br_table of block_symbol list * block_symbol
  | Return
  | Call of function_symbol
  | Call_indirect of type_symbol
[@@deriving sexp]


let rec clambda_to_instruction_list clambda instruction_list = match clambda with
  | Usequence (instr, clambda') -> clambda_to_instruction_list clambda' (instruction_list @ clambda_to_instruction_list instr [])

  | Uvar _ -> failwith "Uvar"
  | Uconst _ -> failwith "Uconst"
  (*
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
  *)
  | _ -> failwith "not implemented yet"