open Support.Error
open Support.Pervasive
open Syntax
open List
open Db
open Print
open Format
open Options
open Primitives
open Eval
open Sys
open Gcenv

(* ---------------------------------------------------------------------- *)
(* Subtyping *)
type subtype_tester = info -> context -> ty -> ty -> term option bool3

type subtype_algorithm = subtype_tester -> subtype_tester


(* Refs to the subtype testing function and the cleanup function *)
let sub : subtype_tester ref =
  let err_fn _ _ _ = err "Subtype not initialized" in ref err_fn

let sub_fini = ref (function () -> ())

let sub_good =
  let f _ _ _ _ = () in ref f

let sub_bad =
  let f _ _ _ _ _ = () in ref f

(**************************)

let rec init_subtype algs dbo file_names =
  match (algs, dbo) with
  | ([],_) ->
      let is_sub fi ctx tyS tyT = Maybe in
      sub := is_sub

  | (alg::_,None) ->
       if file_exists "default.db" then remove "default.db";
       init_subtype algs (Some "default.db") file_names

  | (alg::_,Some(db_name)) -> (
      (* Create DB, and query that first *)
      let db = (new database db_name) in
      db#init();
      if file_names <> [] then List.iter (db # clear_counts) file_names;
      let rec is_sub fi ctx t1 t2 =
        try db # subtype fi ctx t1 t2
        with NotFoundInDB ->
          let result = alg (db # subtype) fi ctx t1 t2 in
          db # declare fi ctx t1 t2 result;
          result
      in
      sub := is_sub;
      
      sub_bad := (fun fi ctx s t u -> db#declare fi ctx s t (False u));
      sub_good := (fun fi ctx s t -> db#declare fi ctx s t True);
      sub_fini := (function () -> db#fini())
    )

(* Main entry points: *)

(* currently do nothing...*)

let invalid_subtype fi ctx s t u  =
  let s = eval_type ctx s in
  let t = eval_type ctx t in
  (!sub_bad) fi ctx s t u

let valid_subtype fi ctx s t =
  let s = eval_type ctx s in
  let t = eval_type ctx t in
  (!sub_good) fi ctx s t

let maybe_tests = ref []
let eval_steps = ref []

let rec print_maybes maybe_tests =
  List.iter
    (fun (ctx, s, t) ->
       print_info (tm_info s);
       force_newline();
       print_subtype_judgement ctx s t;
       force_newline())
    maybe_tests

let num_in_range lo hi l =
  List.length(filter (function x -> (lo <= x & x < hi)) l)

let rec make_histo l buckets =
  match buckets with
  | []           -> []
  | lo::[]       -> (num_in_range lo 100000 l)::[]
  | lo::hi::rest -> (num_in_range lo hi l)::(make_histo l (hi::rest))

let print_int_list l =
  iter (function (x) -> (Format.print_int x); pr " ") l

let make_buckets c n =
  let rec h a =
    if a = n then []
    else a::(h (a+c))
  in
  h 0

let print_stats() =
  if (Options.pmaybe()) then (
    pr "--------------------------------------------";
    force_newline();
    pr "maybe tests:";
    force_newline();
    print_maybes !maybe_tests;
    pr "--------------------------------------------";
    force_newline()
  );
  if (Options.phisto()) then (
    pr "EVAL HISTOGRAM: ";
    print_int_list (make_histo (!eval_steps) (make_buckets 1 100));
    force_newline()
  )

let fini_subtype () = (!sub_fini) (); print_stats()

(********************************************)

let is_subtype fi ctx s t =
  eval_reset_counter();
  let s = eval_type ctx s in
  let t = eval_type ctx t in
  let (ctx', s', t', inv) = gc_env_2 ctx s t in
  let result = (!sub) fi ctx' s' t' in
  if (Options.psub()) then (
      pr "is_subtype: ";
      print_subtype_test ctx s t result;
  );
  if result = Maybe then (
        let (ctx', s', t', _) = gc_env_2 ctx' s' t' in
        maybe_tests := (ctx', s', t') :: !maybe_tests
  );
  eval_steps := (eval_counter())::!eval_steps;
  result
