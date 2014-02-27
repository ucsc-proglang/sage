open Support.Pervasive
open Support.Error

open Syntax

(* a database maps casts to term options, which are None
 * if the casts are known to fail, or the "via" term
 * otherwise. *)
type cast = context * ty * ty

type cast_result = term option bool3

exception NotFoundInDB

class database :
  string ->
  object
    method declare : info -> context -> ty -> ty -> cast_result -> unit
    method fini : unit -> unit
    method get_name : unit -> string
    method init : unit -> unit
    method clear_counts : string -> unit
    method print_state : unit -> unit
    method subtype : info -> context -> ty -> ty -> cast_result
  end
