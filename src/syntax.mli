(** module Syntax : syntax trees and associated support functions *)

open Support.Pervasive
open Support.Error

(** {1 Data type definitions} *)

(** Obsolete data type which we use for carrying random info *)
type label = string
type variable = string
type var_list = variable list

(** A placeholder is replaced during "type" (i.e. term) reconstruction. *)
type placeholder = PlaceHolder of string

(** A substitution that cannot be applied until the placeholder is
    resolved. *)
and delayed_substitution = term list

and term =
  | TmVar of info * int * sanity_check
  | TmFun of info * variable * term * term
  | TmLet of info * variable * term * term
  | TmArrow of info * variable * term * term
  | TmApp of info * term * term 
  | TmPrimitive of info * string * label list
  | TmPlaceHolder of info * placeholder * delayed_substitution
and ty = term
and value = term

(** This is the type of whatever is dragged around with TmVar, enabling
    some useful debugging output.  This type should remain abstract! *)
and sanity_check (* = var_list *)

type binding = VarBind of ty * (term option)

type context = (string * binding) list

(** This type is intended to fully describe the behavior of a constant
    in our langage.  Constants such as integers all share a single
    [prim_info] record, so the name is not included. *)
type prim_info = {
  (** The type of the primitive *)
  ty: ty;
  
  (** [true] if the primitive is a value 
      when it is applied to its full arity, for example [Refine].  Examples of
      [false] include [+] and [-], etc. *)
  is_full_app_value: bool;  

  (** This function will be called once the arity is fulfilled if 
      [is_full_app_value] is false. *)
  eval_fn : (info -> context -> label list -> term list -> term);

  (** This function is called when the constant appears as the target
      type of a cast. *)
  cast_fn:
    (info -> context -> label list -> term list -> term -> value -> term)
}

type command =

  | Eval of info * term                    (** A term to be evaluated. *)

  | Define of info * variable * term       (** A top-level [let] binding. *)

  | Assume of context * term * term * bool (** A subtyping relation to assume 
                                               is true or false.  If we can 
                                               prove or refute it, a warning 
                                               is printed. *)

  | Check of context * term * term * bool  (** Like [Assume] except if we 
                                               can't prove or refute it, 
                                               it is an error. *)

  | Prove of context * term * bool         (** Like check, but just testing 
                                               the theorem prover. *)
type replacement = (context * placeholder * term) list

exception NoRuleApplies of string

(** {2 Contexts} *)

val empty_ctx : context 
val add_binding : context -> variable -> binding -> context
val get_binding : info -> context -> int -> binding
val name_to_index : info -> context -> string -> int
val name_to_term  : info -> context -> string -> term
val index_to_name : info -> context -> int    -> string

(** @deprecated *)
val bind_fresh_name : context -> variable -> binding -> (context * variable)

(** {1 Variable lists} *)

val var_list_from_ctx : context -> var_list
val ctx_from_var_list : var_list -> context
val var_to_index : info -> var_list -> string -> int
val var_to_term  : info -> variable list -> string -> term
val index_to_var : info -> var_list -> int -> string

val pick_fresh_var : var_list -> ?suffix:string -> string ->
                     (var_list * string)

(** {1 Sanity checking of Debruijn} *)

val sanity_check_from_var_list : variable list -> sanity_check
val sanity_check_from_ctx : context -> sanity_check
val check_sane_vars : info -> var_list -> int -> sanity_check -> bool
val check_sane_ctx : info -> context -> int -> sanity_check -> bool
val shift_check : sanity_check -> int -> sanity_check
val add_checked_var : sanity_check -> variable -> sanity_check
val sanity_check_string : info -> var_list -> int -> sanity_check -> string

(** {1 Shifting and substitution} *)
(** See TAPL chapter 6 for definitions of all these *)

(** [term_shift d tm] shifts all debruijn bindings in [tm] by [d] *)
val term_shift : int -> term -> term

(** [binding_shift d bind] shifts the type and optional term in binding by
    [d]; it is a thin wrapper on [term_shift]. *)
val binding_shift : int -> binding -> binding

(** [term_shift distance threshold tm] shift every variable in
    [tm] by [distance] iff its index is greater than or equal to
    [threshold]. *)
val term_shift_above : int -> int -> term -> term

(** [subst_top_tm_in_tm arg body] replaces the top variable (index zero)
    with [arg] every time it occurs in [body] *)
val subst_top_tm_in_tm : term -> term -> term

(** [subst_tm_in_tm index arg body] replaces the variable with debruijn
    index [index] with [arg] each time it occurs in [body] *)
val subst_tm_in_tm : int -> term -> term -> term

val apply_delayed_subst : delayed_substitution -> term -> term
val has_delayed_subst : term -> bool
val apply_replacement : replacement -> term -> term

(** {1 Misc} *)

val tm_info : term -> info
val replace_info : term -> info -> term
val contains_var: term -> int -> bool

(** {1 More Complicated Constructors} *)

(** Makes a bunch of nested lambdas for a function of arbitrary arity
    (including zero) *)
val make_lambda_sequence : 
  info -> variable list -> (variable * ty) list -> ?ret:ty -> term -> term

(** Makes the coercion used by the [AS] construct *)
val make_as : info -> variable list -> term -> ty -> term

(** Makes a bunch of nested arrow terms, analogously to 
    [make_lambda_sequence] *)
val make_arrow_sequence :
  info -> variable list -> (variable * ty) list -> ty -> term

(** {1 Equality of Syntactic structures} *)

val term_eq : term -> term -> bool
val ctx_eq : context -> context -> bool
val binding_eq : binding -> binding -> bool
val delayed_subst_eq : term list -> term list -> bool


(** {1 Complicated Destructors} *)

val get_app_args : term -> term list

(** {1 Misc} *)

val valid_term: context -> term -> bool
val is_arrow : term -> bool
val optimize: term->term

val tm_id_fn : info -> context -> term -> term 
val is_id_fn : term -> bool
