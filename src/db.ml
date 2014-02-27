open Syntax
open Marshal
open Pervasives
open Sys
open List
open Support.Pervasive
open Format
open Primitives
open Print
open Support.Error
open Options
open Gcenv

type cast = context * ty * ty

type cast_result = term option bool3

type db_info = (string * int)
type db_info_list = db_info list

type database_entry = context * ty * ty * cast_result * (db_info_list ref)

exception NotFoundInDB

(****************************************************************)

let result_eq r1 r2 =
  match (r1, r2) with
  | True, True -> true
  | (False _, False _) -> true
  | _ -> false


let rec inc_dbi_info dbi fname =
  match dbi with
  | [] -> [ (fname,1) ]
  | ((s,c)::ds) ->
      if (s = fname) then
        (s,c+1)::ds
      else
        (s,c)::(inc_dbi_info ds fname)

let rec clear_db_info dbi fname =
  match dbi with
  | [] -> [ ]
  | ((s,c)::ds) ->
      if (s = fname) then
        ds
      else
        (s,c)::(clear_db_info ds fname)

let print_db_info_list dbi =
  iter (function x ->
    let (x,c) = x in
    print_string x;
    pr "(";
    print_int c;
    print_string ") ") dbi

let print_cast (ctx, s, t, u, dbi) =
  print_subtype_test ctx s t u;
  pr "Locations: ";
  print_db_info_list (!dbi);
  force_newline();
  force_newline()

let log s = pr s; force_newline()

let entry_matches (ctx,s,t) =
  function (ctx', s', t', _, _) ->
    (ctx_eq ctx ctx') && (term_eq s s') && (term_eq t t')

let print_recompile dbi =
  force_newline();
  pr "-------------------------";
  force_newline();
  pr "DB says to recompile the following:";
  print_db_info_list (dbi);
  force_newline();
  pr "-------------------------";
  force_newline();


(****************************************************************************)

(* Database class: constructed with the name of the file to use.  Will
create a new db if file does not exist *)

class database name =
object (self)
  val name : string = name
  val mutable data : database_entry list = []
  val mutable hits = 0
  val mutable tests = 0

  method subtype (fi:info) (ctx:context) (s:ty) (t:ty) : cast_result =
    tests <- tests + 1;
    try
      let (ctx',s',t',inverted_map) = gc_env_2 ctx s t in
      let (ctx',s',t',u,dbi) = find (entry_matches (ctx', s', t')) data in
          hits <- hits + 1;
          let fname = (filename_of_info fi) in
          dbi := inc_dbi_info !dbi fname;
          u
    with Not_found -> raise NotFoundInDB

  method declare (fi:info) (ctx:context) (s:ty) (t:ty) (u:cast_result) =
    let ctx,s,t,u' =
      match u with
      | True | Maybe -> let ctx, s, t, _ = gc_env_2 ctx s t in (ctx, s, t, u)
      | False None ->
          let ctx, s, t, _ = gc_env_2 ctx s t in (ctx, s, t, u)
      | False (Some counterex) ->
          let ctx, s, t, counterex, _ = gc_env_3 ctx s t counterex in
          (ctx, s, t, False (Some counterex))
    in
       (try
           let c = (ctx, s, t) in
           let (ctx, s, t, u'', dbi) = find (entry_matches c) data in
           if (not (result_eq u' u'')) then
             print_recompile !dbi;
           data <- (filter (function x -> not ((entry_matches c) x)) data)
        with Not_found -> ());
        data <- (ctx, s, t, u',
                 ref (inc_dbi_info [] (filename_of_info fi)))::data

  method clear_counts fname =
    pr "Clearing ";
    pr fname;
    force_newline();
    iter (function x ->
            let (ctx,s,t,u,dbi) = x in
            dbi := (clear_db_info !dbi fname))
      data
      
    method get_name() = name

    method init() =
      if file_exists name then
        let input = open_in_bin name in
        log ("loading database " ^ name ^ " from file.");
        data <- (from_channel input: database_entry list);

        close_in input;
      else
        log ("creating new database " ^ name ^ ".");

    method fini() =
      let output = open_out_bin name in
      log ("writing database " ^ name ^ ".");
      to_channel output data [];
      log(Printf.sprintf "db hits:   %d" hits);
      log(Printf.sprintf "db misses: %d" (tests - hits));
      log(Printf.sprintf "db size: %d" (List.length data));
      close_out output

    method print_state () =
      pr ("State of " ^ name ^ ":\n");
      (* iter print_cast (rev data); *)
      force_newline()

end
