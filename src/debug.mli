
(** This module contains debug printing functions.
    Debugging code is executed dependant on keys specify key with "-d KEY" on command line:  *)
  
type key = string
    
(** Wrapper to prevent unit function from running if key turned off *)
val debug : key -> (unit -> unit) -> unit

(** [debug_pr key str] prints [str] if [key] is turned on *)
val debug_pr : key -> string -> unit

(** See [debug_pr] *)
val debug_pr_newline : key -> string -> unit
    
(** debug_wrapper - execute pre and post functions if key turned on. *)
val debug_wrapper : key -> (unit -> unit) -> ('a -> unit) -> (unit -> 'a) -> 'a

