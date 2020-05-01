open Flambda_frontend.Flambda_types


type inttype = [`Int | `Int32 | `Int64 | `Bigint]

type vector_type =
  [`Array | `Bytevec]

(* value type from malfunction https://github.com/stedolan/malfunction/blob/master/src/malfunction_interpreter.ml *)
type value =
| Block of int * value array
| Vec of vector_type * value array
| Func of (value -> value)
| Int of inttype * Z.t
| Float of float

type symbol = Symbol of int
type continuation_symbol = Continuation of int

type environment = {
  k_return : continuation;
  k_exn : continuation;

  variables  : (symbol * value) list;
  continuations : (continuation_symbol * continuation) list;
}
and continuation = (environment * continuation_argument_symbols * expr)
and continuation_argument_symbols = symbol list



let rec interpret_expr env : t -> value = function

  (** Bind a variable.  There can be no effect on control flow (save for
        asynchronous operations such as the invocation of finalisers or
        signal handlers as a result of reaching a safe point). *)
  | Let {lexpr_bound_vars_and_body; lexpr_defining_expr} ->
    let (bindable_let_bound, body) = lexpr_bound_vars_and_body in
    interpret_expr env' body

  (** Bind code and/or data symbol(s).  This form of expression is only
        allowed in certain "toplevel" contexts.  The bound symbols are not
        treated up to alpha conversion; each such bound symbol must be
        unique across the whole program being compiled. *)
  | Let_symbol {lsymbol_expr_bound_symbols;
    lsymbol_expr_defining_expr;
    lsymbol_expr_body} ->
    interpret_expr env' lsymbol_expr_body

  (** Define one or more continuations. *)    
  | Let_cont lcont_expr -> match lcont_expr with
    | LetContExprNon_recursive {lcont_expr_handler;
        lcont_expr_num_free_occurrences;
    } ->
      let (bindable_cont, body) = (non_recursive_let_cont_handler_continuation_and_body lcont_expr_handler) in
      interpret_expr env' body

    | LetContExprRecursive recursive_let_cont_handlers ->
      let (bindable_continuation, recursive_let_cont_handlers_t0) = recursive_let_cont_handlers in
      interpret_expr env' (recursive_let_cont_handlers_t0_body recursive_let_cont_handlers_t0)


  (** Call an OCaml function, external function or method. *)
  | Apply apply_expr ->

  (** Call a continuation, optionally adding or removing exception trap
        frames from the stack, which thus allows for the raising of
        exceptions. *)
  | Apply_cont apply_cont_expr ->

  (** Conditional control flow. *)
  | Switch switch_expr ->

  | Invalid invalid_term_semantics_expr ->
    fail "interpret Invalid"