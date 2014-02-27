open Syntax

(** Definitions of the primitive constants in Sage, including their types,
    evaluation functions, casting behavior, and status as values.
*)

(** A list containing the definitions of each primitive. See the definition of
    [prim_info] for a description of the format of the entries in this list *)
val prim_definitions : (string * prim_info) list

(** [parse_string_term vars str] parses the term described by [str] in the
    environment where the variables in [vars] are in scope. Ideally, this
    should be in a different module, but restrictions on module recursion
    make this difficult. *)
val parse_term_string : Syntax.var_list -> string -> Syntax.term
    
(** A list of the valid primitive names, for use in the parser. This should
    include the first element of all of the pairs in [prim_definitions]. *)
val prim_name_list : string list
