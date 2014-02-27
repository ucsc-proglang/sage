open Syntax
open Support.Error

(** Type-checked compilation of Sage terms *)

(** [compile ctx tm] returns a pair [tm', ty] where [tm'] is the
    result of compilation and [ty] is its type. *)
val compile_tm : context -> term -> term * term

(** [typeof ctx tm] is valid only for already-compiled terms,
    and returns the memoized type for that term. *)
val typeof : context -> term -> term

(** matches the [unrefine] function in our paper *)
val strip_refine_and_eval : info -> context -> ty -> ty
