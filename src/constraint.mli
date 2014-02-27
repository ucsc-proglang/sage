open Support.Error
open Syntax

type constraint_t =  info * context * term * term

val type_constraints : context -> term -> term * constraint_t list

val greedy_unify : constraint_t list -> replacement

val print_constraint : constraint_t -> unit

