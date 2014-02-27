open Syntax
open Support.Error

(** {6 Primitive Constructors} *)

val tm_prim : 
  ?fi:info -> ?labels:label list -> string -> term

(** Given a string representing an integer, creates a singleton type for that
    integer. *)
val int_singleton : (string -> term) ref
  
(** [make_cast fi ctx tyS tyT tm] creates a cast from type [tyS] to type [tyT]
    of term [tm], in the context [ctx], using [fi] to associate terms in the
    result with a file location. *)
val make_cast : info -> context -> term -> term -> term -> term

(** [make_refinement fi ctx var ty tm] creates the term [Refine ty (fn (x:ty)
    => tm)] * in the context [ctx] and with file info [fi]. *)
val make_refinement : info -> variable list -> variable -> term ->
                      term -> term

(** [make_prim_app_sequence fi ctx pname labels terms] creates an application
    of the primitive given by [pname] to the term list [terms], in context
    [ctx] and with file info [fi]. The label list [labels] fills in the label
    field of the primitive. *)
val make_prim_app_sequence : info -> variable list -> string -> label list ->
    term list -> term

(** {6 Primitive Inspection} *)

(** [get_prim_info fi ctx pname labels] retrieves the [prim_info] structure
    for the primitive denoted by [pname] and [labels]. Exits with an error
    message if the primitive requested does not exist. *)
val get_prim_info : info -> context -> string -> label list -> prim_info

(** Returns the name of the given primitive term. Raises [Invalid_argument] if
    the term is not a primitive. *)
val prim_name : term -> string

(** Returns the labels of the given primitive term. Raises [Invalid_argument]
    if the term is not a primitive. *)
val prim_labels : term -> label list

(** [primitive_ty fi ctx pname labels] returns the type of the primitive
    associated with [pname] and [labels], adjusted for the context [ctx].
    Behaves like [get_prim_info] if the requested primitive does not exist. *)
val primitive_ty : info -> context -> string -> label list -> term

(** Behaves like [primitive_ty], but returns the arity of the primitive,
    rather than its type. *)
val primitive_arity : info -> context -> string -> label list -> int

(** Returns true if the given term is a primitive or an application of a
    primitive, and false otherwise. *)
val is_primitive : term -> bool

(** Returns true if the given term is a primitive with a name equal to the
    given name, or an application of such a primitive, and false otherwise. *)
val is_named_primitive : term -> string -> bool

(** Returns true if the given term is a value, false otherwise. TODO: do we
  * need a context and a file info? *)
val is_val : info -> context -> term -> bool

(** Returns true if the given term is an application of a primitive to its
    full number of arguments, and false otherwise. *)
val is_full_prim_app : info -> context -> term -> bool

(** Given an arrow type of any arity, returns a list of its component terms. 
    TODO: should this be in term.ml? *)
val fully_split_arrow : term -> (variable * term) list

(** {6 Icky stuff} *)

(** A reference to the list of primitives from prim_defs.ml, used to get
    around module recursion limitations. *)
val prim_list : (string * prim_info) list ref

(** A global reference used to get around module recursion limitations. *)
val toplevel_parse_thunk: (string -> Syntax.var_list -> string list ->
                             (Syntax.command list * Syntax.var_list)) ref
