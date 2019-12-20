open Sexplib0
open Sexp_conv

(* Copied from parsing/asttypes.mli *)

type direction_flag = Asttypes.direction_flag = Upto | Downto
[@@deriving sexp]

type mutable_flag = Asttypes.mutable_flag = Immutable | Mutable
[@@deriving sexp]

(* Copied from lambda/lambda.ml *)

type boxed_integer = Lambda.boxed_integer =
    Pnativeint | Pint32 | Pint64
[@@deriving sexp]

type value_kind = Lambda.value_kind =
    Pgenval | Pfloatval | Pboxedintval of boxed_integer | Pintval
[@@deriving sexp]

type meth_kind = Lambda.meth_kind =
    Self | Public | Cached
[@@deriving sexp]

type block_shape = value_kind list option
[@@deriving sexp]

type immediate_or_pointer = Lambda.immediate_or_pointer =
  | Immediate
  | Pointer
[@@deriving sexp]

type initialization_or_assignment = Lambda.initialization_or_assignment =
  | Assignment
  (* Initialization of in heap values, like [caml_initialize] C primitive.  The
     field should not have been read before and initialization should happen
     only once. *)
  | Heap_initialization
  (* Initialization of roots only. Compiles to a simple store.
     No checks are done to preserve GC invariants.  *)
  | Root_initialization
[@@deriving sexp]

type raise_kind = Lambda.raise_kind =
  | Raise_regular
  | Raise_reraise
  | Raise_notrace
[@@deriving sexp]

type is_safe = Lambda.is_safe =
  | Safe
  | Unsafe
[@@deriving sexp]

type integer_comparison = Lambda.integer_comparison =
    Ceq | Cne | Clt | Cgt | Cle | Cge
[@@deriving sexp]

type float_comparison = Lambda.float_comparison =
    CFeq | CFneq | CFlt | CFnlt | CFgt | CFngt | CFle | CFnle | CFge | CFnge
[@@deriving sexp]

type array_kind = Lambda.array_kind =
    Pgenarray | Paddrarray | Pintarray | Pfloatarray
[@@deriving sexp]

type bigarray_kind = Lambda.bigarray_kind =
    Pbigarray_unknown
  | Pbigarray_float32 | Pbigarray_float64
  | Pbigarray_sint8 | Pbigarray_uint8
  | Pbigarray_sint16 | Pbigarray_uint16
  | Pbigarray_int32 | Pbigarray_int64
  | Pbigarray_caml_int | Pbigarray_native_int
  | Pbigarray_complex32 | Pbigarray_complex64
[@@deriving sexp]

type bigarray_layout = Lambda.bigarray_layout =
    Pbigarray_unknown_layout
  | Pbigarray_c_layout
  | Pbigarray_fortran_layout
[@@deriving sexp]

(* Copied from typing/ident.mli *)

type ident = Ident.t
let sexp_of_ident _ = Sexp.Atom "TODO"
let ident_of_sexp _ = failwith "TODO"

(* Copied from middle_end/backend_var.mli *)

type backend_var = ident
[@@deriving sexp]

type backend_var_with_provenance = Backend_var.With_provenance.t
let sexp_of_backend_var_with_provenance _ = Sexp.Atom "TODO"
let backend_var_with_provenance_of_sexp _ = failwith "TODO"

(* Copied from lambda/debuginfo.mli *)

type debuginfo = Debuginfo.t
let sexp_of_debuginfo _ = Sexp.Atom "TODO"
let debuginfo_of_sexp _ = failwith "TODO"

(* Copied from typing/types.mli *)

type path = Path.t =
    Pident of ident
  | Pdot of path * string
  | Papply of path * path
[@@deriving sexp]

type record_representation = Types.record_representation =
    Record_regular            (* All fields are boxed / tagged *)
  | Record_float              (* All fields are floats *)
  | Record_unboxed of bool    (* Unboxed single-field record, inlined or not *)
  | Record_inlined of int               (* Inlined record *)
  | Record_extension of path          (* Inlined record under extension *)
[@@deriving sexp]

(* Copied from typing/primitive.mli *)

type native_repr = Primitive.native_repr =
  | Same_as_ocaml_repr
  | Unboxed_float
  | Unboxed_integer of boxed_integer
  | Untagged_int
[@@deriving sexp]

type primitive_description = Primitive.description = private
  { prim_name: string;         (* Name of primitive  or C function *)
    prim_arity: int;           (* Number of arguments *)
    prim_alloc: bool;          (* Does it allocates or raise? *)
    prim_native_name: string;  (* Name of C function for the nat. code gen. *)
    prim_native_repr_args: native_repr list;
    prim_native_repr_res: native_repr }
let sexp_of_primitive_description _ = Sexp.Atom "TODO"
let primitive_description_of_sexp _ = failwith "TODO"

(* Copied from middle_end/clambda_primitives.mli *)

type memory_access_size = Clambda_primitives.memory_access_size =
  | Sixteen
  | Thirty_two
  | Sixty_four
[@@deriving sexp]

type primitive = Clambda_primitives.primitive =
  | Pread_symbol of string
  (* Operations on heap blocks *)
  | Pmakeblock of int * mutable_flag * block_shape
  | Pfield of int
  | Pfield_computed
  | Psetfield of int * immediate_or_pointer * initialization_or_assignment
  | Psetfield_computed of immediate_or_pointer * initialization_or_assignment
  | Pfloatfield of int
  | Psetfloatfield of int * initialization_or_assignment
  | Pduprecord of record_representation * int
  (* External call *)
  | Pccall of primitive_description
  (* Exceptions *)
  | Praise of raise_kind
  (* Boolean operations *)
  | Psequand | Psequor | Pnot
  (* Integer operations *)
  | Pnegint | Paddint | Psubint | Pmulint
  | Pdivint of is_safe | Pmodint of is_safe
  | Pandint | Porint | Pxorint
  | Plslint | Plsrint | Pasrint
  | Pintcomp of integer_comparison
  | Poffsetint of int
  | Poffsetref of int
  (* Float operations *)
  | Pintoffloat | Pfloatofint
  | Pnegfloat | Pabsfloat
  | Paddfloat | Psubfloat | Pmulfloat | Pdivfloat
  | Pfloatcomp of float_comparison
  (* String operations *)
  | Pstringlength | Pstringrefu  | Pstringrefs
  | Pbyteslength | Pbytesrefu | Pbytessetu | Pbytesrefs | Pbytessets
  (* Array operations *)
  | Pmakearray of array_kind * mutable_flag
  (** For [Pmakearray], the list of arguments must not be empty.  The empty
      array should be represented by a distinguished constant in the middle
      end. *)
  | Pduparray of array_kind * mutable_flag
  (** For [Pduparray], the argument must be an immutable array.
      The arguments of [Pduparray] give the kind and mutability of the
      array being *produced* by the duplication. *)
  | Parraylength of array_kind
  | Parrayrefu of array_kind
  | Parraysetu of array_kind
  | Parrayrefs of array_kind
  | Parraysets of array_kind
  (* Test if the argument is a block or an immediate integer *)
  | Pisint
  (* Test if the (integer) argument is outside an interval *)
  | Pisout
  (* Operations on boxed integers (Nativeint.t, Int32.t, Int64.t) *)
  | Pbintofint of boxed_integer
  | Pintofbint of boxed_integer
  | Pcvtbint of boxed_integer (*source*) * boxed_integer (*destination*)
  | Pnegbint of boxed_integer
  | Paddbint of boxed_integer
  | Psubbint of boxed_integer
  | Pmulbint of boxed_integer
  | Pdivbint of { size : boxed_integer; is_safe : is_safe }
  | Pmodbint of { size : boxed_integer; is_safe : is_safe }
  | Pandbint of boxed_integer
  | Porbint of boxed_integer
  | Pxorbint of boxed_integer
  | Plslbint of boxed_integer
  | Plsrbint of boxed_integer
  | Pasrbint of boxed_integer
  | Pbintcomp of boxed_integer * integer_comparison
  (* Operations on big arrays: (unsafe, #dimensions, kind, layout) *)
  | Pbigarrayref of bool * int * bigarray_kind * bigarray_layout
  | Pbigarrayset of bool * int * bigarray_kind * bigarray_layout
  (* size of the nth dimension of a big array *)
  | Pbigarraydim of int
  (* load/set 16,32,64 bits from a string: (unsafe)*)
  | Pstring_load of (memory_access_size * is_safe)
  | Pbytes_load of (memory_access_size * is_safe)
  | Pbytes_set of (memory_access_size * is_safe)
  (* load/set 16,32,64 bits from a
     (char, int8_unsigned_elt, c_layout) Bigarray.Array1.t : (unsafe) *)
  | Pbigstring_load of (memory_access_size * is_safe)
  | Pbigstring_set of (memory_access_size * is_safe)
  (* byte swap *)
  | Pbswap16
  | Pbbswap of boxed_integer
  (* Integer to external pointer *)
  | Pint_as_pointer
  (* Inhibition of optimisation *)
  | Popaque
[@@deriving sexp]


(* Copied from middle_end/clambda.ml *)

type function_label = string
[@@deriving sexp]

type ustructured_constant = Clambda.ustructured_constant =
  | Uconst_float of float
  | Uconst_int32 of int32
  | Uconst_int64 of int64
  | Uconst_nativeint of nativeint
  | Uconst_block of int * uconstant list
  | Uconst_float_array of float list
  | Uconst_string of string
  | Uconst_closure of ufunction list * string * uconstant list

and uconstant = Clambda.uconstant =
  | Uconst_ref of string * ustructured_constant option
  | Uconst_int of int
  | Uconst_ptr of int

and uphantom_defining_expr = Clambda.uphantom_defining_expr =
  | Uphantom_const of uconstant
  (** The phantom-let-bound variable is a constant. *)
  | Uphantom_var of backend_var
  (** The phantom-let-bound variable is an alias for another variable. *)
  | Uphantom_offset_var of { var : backend_var; offset_in_words : int; }
  (** The phantom-let-bound-variable's value is defined by adding the given
      number of words to the pointer contained in the given identifier. *)
  | Uphantom_read_field of { var : backend_var; field : int; }
  (** The phantom-let-bound-variable's value is found by adding the given
      number of words to the pointer contained in the given identifier, then
      dereferencing. *)
  | Uphantom_read_symbol_field of { sym : string; field : int; }
  (** As for [Uphantom_read_var_field], but with the pointer specified by
      a symbol. *)
  | Uphantom_block of { tag : int; fields : backend_var list; }
  (** The phantom-let-bound variable points at a block with the given
      structure. *)

and ulambda = Clambda.ulambda =
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

and ufunction = Clambda.ufunction = {
  label  : function_label;
  arity  : int;
  params : (backend_var_with_provenance * value_kind) list;
  return : value_kind;
  body   : ulambda;
  dbg    : debuginfo;
  env    : backend_var option;
}

and ulambda_switch = Clambda.ulambda_switch =
  { us_index_consts: int array;
    us_actions_consts: ulambda array;
    us_index_blocks: int array;
    us_actions_blocks: ulambda array}
[@@deriving sexp]

(* Description of known functions *)

type function_description = Clambda.function_description =
  { fun_label: function_label;          (* Label of direct entry point *)
    fun_arity: int;                     (* Number of arguments *)
    mutable fun_closed: bool;           (* True if environment not used *)
    mutable fun_inline: (backend_var_with_provenance list * ulambda) option;
    mutable fun_float_const_prop: bool  (* Can propagate FP consts *)
  }
[@@deriving sexp]

(* Approximation of values *)

type value_approximation = Clambda.value_approximation =
    Value_closure of function_description * value_approximation
  | Value_tuple of value_approximation array
  | Value_unknown
  | Value_const of uconstant
  | Value_global_field of string * int
[@@deriving sexp]

(* Comparison functions for constants *)

type usymbol_provenance = Clambda.usymbol_provenance = {
  original_idents : ident list;
  module_path : path;
}
[@@deriving sexp]

type uconstant_block_field = Clambda.uconstant_block_field =
  | Uconst_field_ref of string
  | Uconst_field_int of int
[@@deriving sexp]

type preallocated_block = Clambda.preallocated_block = {
  symbol : string;
  exported : bool;
  tag : int;
  fields : uconstant_block_field option list;
  provenance : usymbol_provenance option;
}
[@@deriving sexp]

type preallocated_constant = Clambda.preallocated_constant = {
  symbol : string;
  exported : bool;
  definition : ustructured_constant;
  provenance : usymbol_provenance option;
}
[@@deriving sexp]
