open Syntax
open Support.Error
open Support.Pervasive

(** Type of main subtyping function.
    Returns Some(term) when the term should be applied as a cast. It is
    possible that this cast will always succeed and can be optimized away.
    Returns None when it can prove that the subtyping relationship does not
    hold. *)
type subtype_tester = info -> context -> ty -> ty -> term option bool3

(** Type of subtyping algorithms.  An algorithm takes a delegate for 
    doing recursive calls and returns a function matching the type above for
    actually doing the tests. *)
type subtype_algorithm = subtype_tester -> subtype_tester

(** Initialize the subtyping module, passing in the list a algorithms
    to use and the optional database name, and the option file name *)
val init_subtype : 
  subtype_algorithm list -> string option -> string list -> unit

(** Call before exiting to write any state to disk *)
val fini_subtype : unit -> unit

(** Test a subtyping relation *)
val is_subtype : subtype_tester

(** Report that a subtyping relation is invalid *)
val invalid_subtype : info -> context -> ty -> ty -> term option -> unit

(** Report that a subtyping relation is valid *)
val valid_subtype : info -> context -> ty -> ty -> unit

val print_stats:unit -> unit
