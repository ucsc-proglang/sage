open Support.Error
open Syntax

exception NoEvalRule of string * context * term 

val eval_compiler : (context -> term -> (term * term)) ref
val eval_v : context -> term -> term

(* Evaluator with counter that also evals fix at top level *)
val eval_reset_counter : unit -> unit
val eval_counter : unit -> int

val eval_type_many  : int -> context -> ty -> ty
val eval_type : context -> ty -> ty
val simplify_type : context -> ty -> ty
