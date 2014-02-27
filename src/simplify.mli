(**
  The simplify module accepts a simplify formula, calls simplify on formula,
  and returns the response.
*)

open Support.Pervasive

(**
  the method accepts a simplify formula. If Simplify is not already running,
  the module then forks a parallel process which runs the Simplify program.
  Then the simplify formula is piped to Simplify and the program waits for
  and then returns the response.
*)
val call_simplify: string -> string option bool3

