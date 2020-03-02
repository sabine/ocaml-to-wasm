

type expr =
    | Let of let_expr
    (** Bind a variable.  There can be no effect on control flow (save for
        asynchronous operations such as the invocation of finalisers or
        signal handlers as a result of reaching a safe point). *)
    | Let_symbol of let_symbol_expr
    (** Bind code and/or data symbol(s).  This form of expression is only
        allowed in certain "toplevel" contexts.  The bound symbols are not
        treated up to alpha conversion; each such bound symbol must be
        unique across the whole program being compiled. *)
    | Let_cont of let_cont_expr
    (** Define one or more continuations. *)
    | Apply of apply_expr
    (** Call an OCaml function, external function or method. *)
    | Apply_cont of apply_cont_expr
    (** Call a continuation, optionally adding or removing exception trap
        frames from the stack, which thus allows for the raising of
        exceptions. *)
    | Switch of switch_expr
    (** Conditional control flow. *)
    | Invalid of invalid_term_semantics_expr
    (** Code proved type-incorrect and therefore unreachable. *)
and let_expr = {
    let_expr_bound_vars_and_body : bound_vars_and_body;
    let_expr_defining_expr : named;
}
and let_symbol_expr = {
    let_symbol_expr_bound_symbols : bound_symbols;
    let_symbol_expr_defining_expr : static_const;
    let_symbol_expr_body : expr;
}
and let_cont_expr =
    | LetContExprNon_recursive of {
        let_cont_expr_handler : non_recursive_let_cont_handler;
        let_cont_expr_num_free_occurrences : name_occurrences_num_occurrences;
    }
    | LetContExprRecursive of recursive_let_cont_handlers
and apply_expr = {
    apply_expr_callee : simple;
    apply_expr_continuation : continuation;
    apply_expr_exn_continuation : exn_continuation;
    apply_expr_args : simple list;
    apply_expr_call_kind : call_kind;
    apply_expr_dbg : debuginfo;
    apply_expr_inline : inline_attribute;
    apply_expr_inlining_depth : int;
}
and apply_cont_expr = {
    apply_cont_expr_k : continuation;
    apply_cont_expr_args : simple list;
    apply_cont_expr_trap_action : trap_action option;
    apply_cont_expr_dbg : debuginfo;
}
and switch_expr = {
    switch_expr_scrutinee : simple;
    switch_expr_arms : (immediate * continuation) list; (* this was a Map *)
}
and invalid_term_semantics_expr =
    | Treat_as_unreachable
    | Halt_and_catch_fire

and bound_vars_and_body = bindable_let_bound * expr
and bindable_let_bound = 
    | BindableLetBoundSingleton of var_in_binding_pos
    | BindableLetBoundSet_of_closures of {
        bindable_let_bound_name_mode : name_mode;
        bindable_let_bound_closure_vars : (closure_id * var_in_binding_pos) list;
    }

and named = 
    | NamedSimple of simple
    | NamedPrim of flambda_primitive * debuginfo
    | NamedSet_of_closures of set_of_closures

and bound_symbols =
    | BoundSymbolsSingleton of symbol
    | BoundSymbolsSets_of_closures of code_and_set_of_closures list

and tag_scannable = int

and 'a or_variable =
    | OrVariableConst of 'a
    | OrVariableVar of variable

and numbers_float_by_bit_pattern = Int64.t

and targetint = Int64 (* WebAssembly has 64-bit integers, so let's use them *)

and static_const = 
    | StaticConstBlock of tag_scannable * mutable_or_immutable * (field_of_block list)
    | StaticConstSets_of_closures of code_and_set_of_closures list
    | StaticConstBoxed_float of numbers_float_by_bit_pattern or_variable
    | StaticConstBoxed_int32 of Int32.t or_variable
    | StaticConstBoxed_int64 of Int64.t or_variable
    | StaticConstBoxed_nativeint of targetint or_variable
    | StaticConstImmutable_float_array of numbers_float_by_bit_pattern or_variable list
    | StaticConstMutable_string of { initial_value : string; }
    | StaticConstImmutable_string of string

and non_recursive_let_cont_handler = {
    non_recursive_let_cont_handler_continuation_and_body : continuation_and_body;
    non_recursive_let_cont_handler_handler : continuation_handler;
}
and name_occurrences_num_occurrences =
    | NameOccurrencesNumOccurrencesZero
    | NameOccurrencesNumOccurrencesOne
    | NameOccurrencesNumOccurrencesMore_than_one
and recursive_let_cont_handlers = (bindable_continuation * recursive_let_cont_handlers_t0) list
and recursive_let_cont_handlers_t0 = {
        recursive_let_cont_handlers_t0_handlers : continuation_handlers;
        recursive_let_cont_handlers_t0_body : expr;
    }

and simple =
    | SimpleNaked_immediate of immediate
    | SimpleTagged_immediate of immediate
    | SimpleNaked_float of numbers_float_by_bit_pattern
    | SimpleNaked_int32 of Int32.t
    | SimpleNaked_int64 of Int64.t
    | SimpleNaked_nativeint of targetint

and continuation = {
    continuation_id : int;
    (** [id]s are unique within any given compilation unit. *)
    continuation_compilation_unit : compilation_unit;
    continuation_sort : continuation_sort;
}

and continuation_sort = 
    | ContinuationSortNormal
    | ContinuationSortReturn
    | ContinuationSortDefine_root_symbol
    | ContinuationSortToplevel_return
    | ContinuationSortExn

and exn_continuation = {
    exn_continuation_exn_handler : continuation;
    exn_continuation_extra_args : (simple * flambda_kind) list;
}

and bindable_continuation = continuation
and continuation_and_body = bindable_continuation * expr

and continuation_handler = {
    continuation_handler_params_and_handler : continuation_params_and_handler;
    continuation_handler_stub : bool;
    continuation_handler_is_exn_handler : bool;
}
and continuation_handlers = continuation_handler list

and continuation_params_and_handler = (kinded_parameter * expr) list
and kinded_parameter = {
    kinded_parameter_param : parameter;
    kinded_parameter_kind : flambda_kind;
}
and parameter = {
    parameter_var : variable;
}

and value_kind = 
    | Anything
    | Definitely_pointer
    | Definitely_immediate

and flambda_kind =
    | Value
    | Naked_number of naked_number_kind
    | Fabricated

and naked_number_kind =
    | Naked_immediate
    | Naked_float
    | Naked_int32
    | Naked_int64
    | Naked_nativeint

and flambda_kind_standard_int =
    | FlambdaKindStandardIntTagged_immediate
    | FlambdaKindStandardIntNaked_immediate
    | FlambdaKindStandardIntNaked_int32
    | FlambdaKindStandardIntNaked_int64
    | FlambdaKindStandardIntNaked_nativeint

and flambda_kind_standard_int_or_float =
    | FlambdaKindStandardIntOrFloatTagged_immediate
    | FlambdaKindStandardIntOrFloatNaked_immediate
    | FlambdaKindStandardIntOrFloatNaked_float
    | FlambdaKindStandardIntOrFloatNaked_int32
    | FlambdaKindStandardIntOrFloatNaked_int64
    | FlambdaKindStandardIntOrFloatNaked_nativeint

and flambda_kind_boxable_number = 
    | FlambdaKindBoxableNumberNaked_float
    | FlambdaKindBoxableNumberNaked_int32
    | FlambdaKindBoxableNumberNaked_int64
    | FlambdaKindBoxableNumberNaked_nativeint
    | FlambdaKindBoxableNumberUntagged_immediate

and call_kind =
    | Function of function_call
    | Method of { kind : method_kind; obj : simple; }
    | C_call of {
        alloc : bool;
        param_arity : flambda_arity;
        return_arity : flambda_arity;
    }
and method_kind = Self | Public | Cached
and function_call = 
    | Direct of {
        code_id : code_id;
        closure_id : closure_id;
        return_arity : flambda_arity;
    }
    | Indirect_unknown_arity
    | Indirect_known_arity of {
        param_arity : flambda_arity;
        return_arity : flambda_arity;
    }
and flambda_arity = flambda_kind list

and debuginfo = debuginfo_item list
and debuginfo_item = {
    dinfo_file: string;
    dinfo_line: int;
    dinfo_char_start: int;
    dinfo_char_end: int;
    dinfo_start_bol: int;
    dinfo_end_bol: int;
    dinfo_end_line: int;
}

and inline_attribute =
    | Always_inline
    | Never_inline
    | Unroll of int
    | Default_inline

and trap_action =
    | Regular
    | Reraise
    | No_trace

and targetint_ocaml = unit (* TODO *)
and immediate = {
    value : targetint_ocaml;
    print_as_char : bool;
}

and var_in_binding_pos = {
    var : variable;
    name_mode : name_mode;
}
and variable = {
    name : string;
    name_stamp : int;
    (** [name_stamp]s are unique within any given compilation unit.
        We don't need to store the compilation unit because no variables are
        free in a complete program.  Any variables seen (which might e.g. have
      come from another compilation unit) will always be freshened first. *)
    user_visible : bool;
}
and name_mode = 
    | Normal
    | Phantom
    | In_types

and closure_id = {
    closure_id_compilation_unit : compilation_unit;
    closure_id_name : string;
    closure_id_name_stamp : int;
  (** [name_stamp]s are unique within any given compilation unit. *)
}

and block_size =
    | Known of int
    | Unknown

and code_id = {
    id : int * string;
    unit : compilation_unit;
}
and symbol = {
    symbol_compilation_unit : compilation_unit;
    symbol_linkage_name : linkage_name;
    symbol_hash : int;
}
and tag = int
and linkage_name = string
and ident =
    | Local of { name: string; stamp: int }
    | Scoped of { name: string; stamp: int; scope: int }
    | Global of string
    | Predef of { name: string; stamp: int }
    (* the stamp is here only for fast comparison, but the name of
        predefined identifiers is always unique. *)

and compilation_unit = {
    compilation_unit_id : ident;
    compilation_unit_linkage_name : linkage_name;
    compilation_unit_hash : int;
}

and function_declaration = {
    code_id : code_id;
    params_arity : flambda_arity;
    result_arity : flambda_arity;
    stub : bool;
    dbg : debuginfo;
    inline : inline_attribute;
    is_a_functor : bool;
    recursive : recursive;
}

and recursive = Non_recursive | Recursive

and function_declarations = {
  funs : (closure_id * function_declaration) list
}

and var_within_closure = {
    var_within_closure_compilation_unit : compilation_unit;
    var_within_closure_name : string;
    var_within_closure_name_stamp : int;
    (** [name_stamp]s are unique within any given compilation unit. *)
}


and set_of_closures = {
    set_of_closures_function_decls : function_declarations;
    set_of_closures_closure_elements : (var_within_closure * simple) list;
}

and code_and_set_of_closures = {
    code_and_set_of_closures_code_ids : code_id list; (*Code_id.Set.t;*)
    code_and_set_of_closures_closure_symbols : (closure_id * symbol) list;
}

and field_of_block =
    | FieldOfBlockSymbol of symbol
    | FieldOfBlockTagged_immediate of immediate
    | FieldOfBlockDynamically_computed of variable


and mutable_or_immutable =
    | Immutable
    | Mutable

and flambda_primitive =
    | Unary of unary_primitive * simple
    | Binary of binary_primitive * simple * simple
    | Ternary of ternary_primitive * simple * simple * simple
    | Variadic of variadic_primitive * (simple list)

and effects_mutable_or_immutable =
    | EffectsMutableOrImmutableImmutable
    | EffectsMutableOrImmutableMutable

and unary_primitive =
    | Duplicate_block of {
        kind : duplicate_block_kind;
        source_mutability : effects_mutable_or_immutable;
        destination_mutability : effects_mutable_or_immutable;
    }
    | Is_int
    | Get_tag
    | Array_length of block_access_kind
    | Bigarray_length of { dimension : int; }
    | String_length of string_or_bytes
    | Int_as_pointer
    | Opaque_identity
    | Int_arith of flambda_kind_standard_int * unary_int_arith_op
    | Float_arith of unary_float_arith_op
    | Num_conv of {
        src : flambda_kind_standard_int_or_float;
        dst : flambda_kind_standard_int_or_float;
    }
    | Boolean_not
    | Unbox_number of flambda_kind_boxable_number
    | Box_number of flambda_kind_boxable_number
    | Select_closure of {
        move_from : closure_id;
        move_to : closure_id;
    }
    | Project_var of {
        project_from : closure_id;
        var : var_within_closure;
    }
and binary_primitive =
    | BinaryPrimitiveBlock_load of block_access_kind * effects_mutable_or_immutable
    | BinaryPrimitiveString_or_bigstring_load of string_like_value * string_accessor_width
    | BinaryPrimitivePhys_equal of flambda_kind * equality_comparison
    | BinaryPrimitiveInt_arith of flambda_kind_standard_int * binary_int_arith_op
    | BinaryPrimitiveInt_shift of flambda_kind_standard_int * int_shift_op
    | BinaryPrimitiveInt_comp of flambda_kind_standard_int * signed_or_unsigned
        * ordered_comparison
    | BinaryPrimitiveFloat_arith of binary_float_arith_op
    | BinaryPrimitiveFloat_comp of comparison
and ternary_primitive = 
    | TernaryPrimitiveBlock_set of block_access_kind * init_or_assign
    | TernaryPrimitiveBytes_or_bigstring_set of bytes_like_value * string_accessor_width
and variadic_primitive = 
    | VariadicPrimitiveMake_block of make_block_kind * effects_mutable_or_immutable
    | VariadicPrimitiveBigarray_set of is_safe * num_dimensions * bigarray_kind * bigarray_layout
    | VariadicPrimitiveBigarray_load of is_safe * num_dimensions * bigarray_kind * bigarray_layout


(* unary primitive types *)
and generic_array_specialisation = 
    | GenericArraySpecialisationNo_specialisation
    | GenericArraySpecialisationFull_of_naked_floats
    | GenericArraySpecialisationFull_of_immediates
    | GenericArraySpecialisationFull_of_arbitrary_values_but_not_floats

and duplicate_block_kind =
    | DuplicateBlockKindFull_of_values_known_length of tag_scannable
    | DuplicateBlockKindFull_of_values_unknown_length of tag_scannable
    | DuplicateBlockKindFull_of_naked_floats of { length : targetint_ocaml option; }
    | DuplicateBlockKindGeneric_array of generic_array_specialisation

and string_or_bytes = String | Bytes
and unary_int_arith_op = Neg | Swap_byte_endianness
and unary_float_arith_op = F_Abs | F_Neg

(* binary primitives types *)
and string_like_value =
    | StringLikeValueString
    | StringLikeValueBytes
    | StringLikeValueBigstring
and string_accessor_width =
    | Eight
    | Sixteen
    | Thirty_two
    | Sixty_four
and equality_comparison = Eq | Neq
and binary_int_arith_op =
    | Add | Sub | Mul | Div | Mod | And | Or | Xor
and int_shift_op = Lsl | Lsr | Asr
and binary_float_arith_op = F_Add | F_Sub | F_Mul | F_Div
and signed_or_unsigned =
    | Signed
    | Unsigned
and ordered_comparison = OC_Lt | OC_Gt | OC_Le | OC_Ge
and comparison = C_Eq | C_Neq | C_Lt | C_Gt | C_Le | C_Ge

and block_access_kind_t0 =
    | BlockAccessKindT0Value of value_kind
    | BlockAccessKindT0Naked_float

and block_access_kind =
    | BlockAccessKindBlock of { elt_kind : block_access_kind_t0; tag : tag; size : block_size; }
    | BlockAccessKindArray of block_access_kind_t0
    | BlockAccessKindGeneric_array of generic_array_specialisation


(* ternary primitives types *)
and init_or_assign = Initialization | Assignment
and bytes_like_value =
    | BytesLikeValueBytes
    | BytesLikeValueBigstring

(* variadic primitives types *)
and make_block_kind =
    | MakeBlockKindFull_of_values of tag_scannable * (value_kind list)
    | MakeBlockKindFull_of_naked_floats
    | MakeBlockKindGeneric_array of generic_array_specialisation
and is_safe = Safe | Unsafe
and num_dimensions = int
and bigarray_kind =
    | BigarrayKindUnknown
    | BigarrayKindFloat32 | BigarrayKindFloat64
    | BigarrayKindSint8 | BigarrayKindUint8
    | BigarrayKindSint16 | BigarrayKindUint16
    | BigarrayKindInt32 | BigarrayKindInt64
    | BigarrayKindInt_width_int | BigarrayKindTargetint_width_int
    | BigarrayKindComplex32 | BigarrayKindComplex64
and bigarray_layout =
    | BigarrayLayoutUnknown
    | BigarrayLayoutC
    | BigarrayLayoutFortran