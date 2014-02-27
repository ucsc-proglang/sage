open Support.Pervasive
open Support.Error
open Syntax

(** Functions for printing terms, bindings, contexts, et cetera, and for
    converting terms to strings. *)
  
(** [print_tm vars tm] prints the term [tm] to standard output, using [vars]
    to map variable indices to names. *)
val print_tm : var_list -> term -> unit

(** [print_binding ctx binding] prints a string representation of [binding] to
    standard output, using the context [ctx] to look up variables. Should
    [ctx] be a variable list, instead? *)
val print_binding : context -> binding -> unit

(** Print the names, types, and possibly terms in a context to standard
    output *)
val print_ctx : context -> unit

(** Convert a replacement to textual form and print to standard output. *)
val print_replacement : replacement -> unit

(** [print_str_tm str ctx tm] prints the the string [str] to standard output,
    followed by the textual representation of term [tm] in context [ctx]. *)
val print_str_tm : string -> context -> term -> unit

(** Alias for [Format.print_break 0 0]. *)
val break: unit -> unit

(** [print_subtype_judgement ctx s t] Prints just ctx :- s <: t *)
val print_subtype_judgement : context -> ty -> ty -> unit

(** [print_subtype_result ctx b] Print just "Yes", "Maybe", or "No",
    including the optional counterexample. *)
val print_subtype_result : context -> term option bool3 -> unit

(** [print_subtype_test ctx s t b] is a trivial wrapper on the
    above two functions. *)
val print_subtype_test : context -> ty -> ty -> term option bool3 -> unit

(** [string_of_tm buf vars tm] converts the term [tm] into a string, in the
    context described by [vars], and appends the result to the contents of
    pre-allocated buffer [buf]. *)
val string_of_tm : Buffer.t -> var_list -> term -> unit
