open Syntax
open Support.Pervasive

type rev_map

(** Garbage-collects the context according to the root set contained in
    the term list, and returns a new context and list of fixed-up
    terms *)
val gc_env_list : context -> term list -> (context * term list * rev_map)

(** Convenience wrapper to gc_env_list on 2 elements *)
val gc_env_2 : context -> term -> term 
  -> (context * term * term * rev_map)

(** Convenience wrapper to gc_env_list on 3 elements *)
val gc_env_3 : context -> term -> term -> term 
  -> (context * term * term * term * rev_map)

val ungc_env : rev_map -> int -> context -> term -> term
