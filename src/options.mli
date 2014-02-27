(** The options module handles the arguments through the parse_args
    method. *)

(** parse_args - parses command-line args, returns list of filenames
    to process (may be empty) *)
val parse_args : unit -> string list

(** {6 Accessor Thunks for Options}
    Access values of all command-line options through thunks. See the arg_defs
    list in the source file options.mli to see meanings of each option. *)
val search_path: unit -> string list
val infer: unit -> bool
val db: unit -> string option
val psub: unit -> bool
val peval: unit -> bool
val pcasts: unit -> bool
val csteps: unit -> bool
val phisto: unit -> bool
val pqph: unit -> bool
val maxeval: unit -> int
val stupid: unit -> bool
val evalcomp: unit -> bool
val perfect: unit -> bool
val norefine: unit -> bool
val gcenv: unit -> bool
val loadprelude: unit -> bool
val prelude: unit -> string
val nosimplify: unit -> bool
val simplifytimeout: unit -> int
val pmaybe: unit -> bool
val checkassumes: unit -> bool
val noeval: unit -> bool
val slog: unit -> bool
val prover: unit -> string
val prover_options: unit -> string
val steps: unit -> bool
val nopenv: unit -> bool
val width: unit -> int
val basicsteps: unit -> int


(** {6 Debug wrapper code}
    debugging code executed dependant on keys 
    specify key with "-d KEY" on command line: 
    Always execute body.
*)
type key = string

(** wrapper to allow unit function to execute only if key is on *)
val debug : key -> (unit -> unit) -> unit

(** print string if key turned on *)
val debug_pr : key -> string -> unit

(** same as [debug_pr] but adds newline *)
val debug_pr_newline : key -> string -> unit

(** executes all three functions if key turned on (in the order 1 3 2) but
   only the last function otherwise. *)
val debug_wrapper : key -> (unit -> unit) -> ('a -> unit) ->
                    (unit -> 'a) -> 'a

(** {6 at_exit support } *)

(** add an at_exit function *)
val at_exit : (unit -> unit) -> unit

(** calls at_exit functions *)
val call_at_exits : unit -> unit
