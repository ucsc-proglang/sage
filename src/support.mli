(** 
   Collects a number of low-level facilities used by the other modules
   in the typechecker/evaluator. Aside from the general Support module
   there are two submodules: Pervasive and Error.
*)


(** a version of fold_left where the fold function requires 
    knowledge of the current element's index in the array as 
    the first argument. The last element is labeled as 1. *)
val foldi_left : (int -> 'a -> 'b -> 'a) -> 'a -> 'b list -> 'a

(** creates a list of integers which is the integers between 
    0 and n where n is the length of the input list *)
val mapi : (int -> 'a -> 'b) -> 'a list -> 'b list

(** creates a list of integers which is the integers between 
    0 and n where n is the length of the input list *)
val countlist : 'a list -> int list 

(** a version of map which like foldi_left the function requires
    knowledge of the current element's index in the array as
    the first argument. The last element is labeles as 1. *)
val split3 : ('a * 'b * 'c) list -> ('a list) * ('b list) * ('c list)

(** returns whether the string is a digit '0', through '9' *)
val is_numeric : string -> bool 

(**Some pervasive abbreviations -- opened everywhere by convention *)
module Pervasive : 
sig
  (** a three valued boolean type returned by sformula and subtyping *)
  type 'counterexample bool3 = True |False of 'counterexample |Maybe

  (** the utility for printing. It is important to use this function because
      error formatting in Options.debug_wrapper depends on certain things
      holding true. *)
  val pr : string -> unit

end  

(** Error printing utilities -- opened everywhere by convention *)
module Error : sig

  (** An exception raised by the low-level error printer; exported
      here so that it can be caught in module Main and converted into
      an exit status for the whole program. *)
  exception Exit of int

  (** {3 INFO} *)

  (** An element of the type info represents a "file position": a 
      file name, line number, and character position within the line.  
      Used for printing error messages. *)
  type info

  (** a dummy element of type info which contains no relevant information *)
  val dummyinfo : info

  (** takes an object of type info and turns it into a string type. 
      Currently used as a label hack in primitives.ml. *)
  val string_of_info : info -> string

  (* takes in filename, line #, column # and returns appropriate
     info. object *)
  val create_info : string -> int -> int -> info

  (** prints info object. *)
  val print_info : info -> unit

  (** {3 Info accessor functions} *)

  (** returns filename of info *)
  val filename_of_info : info -> string

  (** returns line # of info *)
  val lineno_of_info : info -> int

  (** returns column # of info *)
  val column_of_info : info -> int

  (** A convenient datatype for a "value with file info."  Used in
      the lexer and parser. *)
  type 'a withinfo = {i : info; v : 'a}

  (** PRINT&FAIL *)

  (** Print an error message and fail. The printing function is called
      in a context where the formatter is processing an hvbox.  Insert
      calls to Format.print_space to print a space or, if necessary,
      break the line at that point. *)
  val errf : (unit->unit) -> 'a

  (** Similar to [errf] but prints 'Error' and the information given first *)
  val errf_at : info -> (unit->unit) -> 'a

  (** Similar to [errf] , for the common case where the
      action to be performed is just to print a given string *)
  val err : string -> 'a

  (** Similar to [errf_at], for the common case where the
      action to be performed is just to print a given string *)
  val error : info -> string -> 'a

  (** Prints a message but does not fail afterwards. *)
  val warnf : (unit->unit) -> unit

  (** Similar to [warnf] but prints 'warning' and info first. *)
  val warnf_at : info -> (unit->unit) -> unit

  (** Similar to [warnf] but takes in a string *)
  val warn : string -> unit

  (** Similar to [warn_at] but takes in string. *)
  val warning : info -> string -> unit

  (** warnings generated so far *)
  val num_warnings : unit -> int

end
