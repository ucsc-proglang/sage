open Support.Error
open List
open Support.Pervasive
open Format
open Unix

(***************************************)

let option_searchpath = ref [""]
let option_infer = ref false
let option_db : string option ref = ref None
let option_gcenv = ref false
let option_checkassumes = ref false
let option_csteps = ref false
let option_psub = ref false
let option_peval = ref false
let option_pcasts = ref false
let option_pqph = ref false
let option_phisto = ref false
let option_pmaybe = ref false
let option_maxeval = ref 200
let option_stupid = ref false
let option_perfect = ref false
let option_norefine = ref false
let option_evalcomp = ref false
let option_loadprelude = ref false
let option_prelude = ref "prelude.f"
let option_nosimplify = ref false
let option_simplifytimeout = ref 45
let option_noeval = ref false
let option_slog = ref false
let option_prover = ref "Simplify"
let option_prover_options = ref " -nosc"
let option_steps = ref false
let option_nopenv = ref false
let option_width = ref 67
let option_basicsteps = ref 100

let option_debug_keys = ref []
let option_at_exits = ref []

(***************************************)

let search_path() = !option_searchpath
let infer() = !option_infer
let db() = !option_db
let psub() = !option_psub
let peval() = !option_peval
let pcasts() = !option_pcasts
let csteps() = !option_csteps
let phisto() = !option_phisto
let pqph() = !option_pqph
let maxeval() = !option_maxeval
let stupid() = !option_stupid
let evalcomp() = !option_evalcomp
let perfect() = !option_perfect
let norefine() = !option_norefine
let gcenv() = !option_gcenv
let loadprelude() = !option_loadprelude
let prelude() = !option_prelude
let nosimplify() = !option_nosimplify
let simplifytimeout() = !option_simplifytimeout
let pmaybe() = !option_pmaybe
let checkassumes() = !option_checkassumes
let noeval() = !option_noeval
let slog() = !option_slog
let prover() = !option_prover
let prover_options() = !option_prover_options
let steps() = !option_steps
let width() = !option_width
let basicsteps() = !option_basicsteps
let nopenv() = !option_nopenv

(**************************************)

let arg_defs = [
  "-I",
  Arg.String (fun f -> option_searchpath := f::!option_searchpath),
  " Append a directory to the search path";

  "-infer", Arg.Set option_infer,
  " Activate type inference.";

  "-db",
  Arg.String (fun db_name -> option_db := Some(db_name)),
  " Use database with given name";

  "-slog",
  Arg.Set option_slog,
  " Write log files for simplify queries";

  "-gcenv",
  Arg.Set option_gcenv,
  " GC environments for database";

  "-csteps", Arg.Set option_csteps,
  " Print compilation steps.";

  "-psub", Arg.Set option_psub,
  " Print subtyping checks.";

  "-peval", Arg.Set option_peval,
  " Print evaluation steps.";

  "-pcasts", Arg.Set option_pcasts,
  " Print casts.";

  "-pqph", Arg.Set option_pqph,
  " Print only a ? for placeholders and their substitutions.";

  "-pmaybe", Arg.Set option_pmaybe,
  " Print any maybe results from the subtype algorithm.";

  "-phisto", Arg.Set option_phisto,
  " Print the number of eval steps in each subtype query.";

  "-steps", Arg.Set option_steps,
  " Prints source, compilation, evaluation of each command.";

  "-maxeval", Arg.Int (fun steps -> option_maxeval := steps),
  " Set the max. number of steps allowed to evaluate types at compile time.";

  "-stupid", Arg.Set option_stupid,
  " Subtyping Algorithm always returns the cast";

  "-perfect", Arg.Set option_perfect,
  " Error if any subtype relation cannot be verified";

  "-norefine", Arg.Set option_norefine,
  " Algorithm ignores refinement types";

  "-evalcomp", Arg.Set option_evalcomp,
  " Compile intermediate evaluation steps.";

  "-loadprelude", Arg.Set option_loadprelude,
  " Load the standard prelude of definitions.";

  "-prelude", Arg.String (fun f -> option_prelude := f),
  " Set the location of the prelude file.";

  "-nosimplify", Arg.Set option_nosimplify,
  " Ask the subtyping algorithm to ignore simplify";

  "-simplifytimeout", Arg.Int
                        (fun timeout ->
                           option_simplifytimeout := timeout),
  " Set # secs simplify should give a problem. Default 45 sec.";

  "-prover", Arg.String (fun f -> option_prover := f),
  " command to invoke Simplify.";

  "-prover_option", Arg.String
                      (fun f ->
                         option_prover_options :=
                         !option_prover_options ^ " " ^ f),
  " command-line argument for simplify.";

  "-checkassumes", Arg.Set option_checkassumes,
  " Check assumes with subtyping algorithm";

  "-nopenv", Arg.Set option_nopenv,
  " Do not print environments";

  "-width", Arg.Int (fun w -> option_width := w),
  " Width of screen";

  "-basicsteps", Arg.Int (fun w -> option_basicsteps := w),
  " Eval steps taken on each call to basic alg ";

  "-d",
  Arg.String (fun f -> option_debug_keys := f::!option_debug_keys),
  " Turns on debugging for given key";

  "-nd",
  Arg.String (fun f -> option_debug_keys :=
    (List.filter (function x -> not (x = f)) !option_debug_keys)),
  " Turns off debugging for given key";

  "-noeval", Arg.Set option_noeval,
  " Don't evaluate the program";
]

let parse_args () =
  let in_files = ref [] in
  Arg.parse ( (* Arg.align  *)
    arg_defs)
    (fun s -> in_files := !in_files @ [s])
    "";
  !in_files

(*******************************)

type key = string

let debug_on key =
  ((mem "all" !option_debug_keys) || (mem key !option_debug_keys))

let debug key f =
  if debug_on key then f() else ()

let debug_pr key str =
  if debug_on key then (pr str) else ()

let debug_pr_newline key str =
  if debug_on key then (pr str; force_newline()) else ()

let counter = ref(0)

let debug_wrapper (id :key) (intro : unit ->unit) (concl : 'a -> unit)
    (fn : unit -> 'a) : 'a =
  if debug_on id
  then
    begin
      let ind = !counter in
      counter := ind + 1;
      open_box 4;
      print_int ind; pr " >> ";
      intro();
      let result = fn() in
      close_box();
(*      if (!counter - 1) = ind
      then close_box();*)
      force_newline();
      print_int ind; pr " << ";
      concl result;
      print_cut();
      result
    end
  else fn()

(********************************)

let at_exit fn =
  option_at_exits := fn::!option_at_exits

let call_at_exits () =
  let rec doit l =
    match l with
    | [] -> ()
    | fn::fs -> fn(); doit fs
  in
  doit (!option_at_exits)
