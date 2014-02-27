open Format
open Support.Error
open Support.Pervasive
open List
open Syntax
open Options

(* ---------------------------------------------------------------------- *)
(* Printing *)

(* The printing functions call these utility functions to insert grouping
  information and line-breaking hints for the pretty-printing library:
     obox   Open a "box" whose contents will be indented by two spaces if
            the whole box cannot fit on the current line
     obox0  Same but indent continuation lines to the same column as the
            beginning of the box rather than 2 more columns to the right
     cbox   Close the current box
     break  Insert a breakpoint indicating where the line maybe broken if
            necessary.
  See the documentation for the Format module in the OCaml library for
  more details.
*)


let obox0() = open_hovbox 0
let obox() = open_hovbox 2
let cbox() = close_box()
let break() = print_break 0 0

let small t =
  match t with
  | TmVar(_, _, _) -> true
  | _ -> false

let rec print_tm_helper outer vars t =
  obox0();
  (match t with
  | TmFun(fi, x, tyT1, t2) ->
      let rec pr_fun outer vars t =
            (match t with
             | TmFun(fi, x, tyT1, t2) ->
                 let (vars', x') = (pick_fresh_var vars x) in
                 (print_space();
              pr "(";
                  pr x';
                  pr ":";
                  print_tm_helper false vars tyT1;
                  pr ")" );
                 pr_fun outer vars' t2
             | _ ->
                 pr " =>";
                 print_space();
                 print_tm_helper outer vars t)
      in
      obox(); pr "(fn"; pr_fun outer vars t; pr ")"; cbox()
  | TmLet(fi, x, t1, t2) ->
      (let (vars', x') = (pick_fresh_var vars x) in
      obox(); pr "(let ";
      pr x'; pr " = "; print_tm_helper outer vars t1; pr " in ";
      if (small t2) && not outer then break() else print_space();
      print_tm_helper outer vars' t2;
      pr ")";
      cbox())
  | TmArrow(fi, x, tyS, tyT) ->
      let rec pr_arrow outer vars t =
        (match t with
        | TmArrow(fi, x, tyS, tyT) ->
            (let (vars', x') = (pick_fresh_var vars x) in
            (if not(x = "_") then (pr x'; pr ":"));
            print_tm_helper false vars tyS;
        print_space ();
            pr "->";
        print_space ();
            pr_arrow false vars' tyT);
        | _ -> print_tm_helper outer vars t)
      in
      obox();
      pr "(";
      pr_arrow outer vars t;
      pr ")";
      cbox()

  (* TODO: allow each primitive to define its own
     printing function - it may have
     bad input, but should still be printable, probably
     by throwing an exception and letting this main function
     print the raw term *)
(*
  | TmApp(fi,
          TmApp(_,
                TmPrimitive(_, "Refine", _),
                ty),
          TmFun(_,x,ty',body))

          -> (
            pr "(";
            pr x;
            pr ":"; print_tm_helper outer vars ty; pr ".";
            obox();
            print_tm_helper outer (x::vars) body;
            cbox();
            pr ")")
*)

  | TmApp(fi, t1, t2) ->
      pr "(";
      obox0();
      print_app_sequence vars t;
      pr ")";
      cbox()
  | TmVar(fi, x, check) ->
      if check_sane_vars fi vars x check  then
        pr (index_to_var fi vars x)
      else
        pr ("[******************bad index: "
            ^ (sanity_check_string fi vars x check) ^ "]")

  | TmPrimitive(fi, name, _) -> pr name

  | TmPlaceHolder(fi, PlaceHolder p, subst) ->

      let process_one_subst initial x tm =
        match tm with
          | TmVar (_,y,_) when x = y -> false
          | tm ->
              if initial then pr "[";
              (* pr (index_to_var fi vars x);
              pr " := "; *)
              print_tm_helper false vars tm;
              true
      in

      let rec print_subst initial index s =
        match s with
          | [] -> ();
          | [tm] ->
              let printed = process_one_subst initial index tm in
              if printed || (not initial) then
                pr "]";

          | tm :: tl ->
              let printed = process_one_subst initial index tm in
              if printed then pr ", ";
              print_subst (initial && (not printed)) (index+1) tl
      in

      pr "/*PH*/Dynamic"
  );
  cbox()

(* This function assumes that parentheses have already been
   opened around a sequence of applications. *)
and print_app_sequence vars t =
  match t with
  | TmApp (fi,t1,t2) ->
      obox0 ();
      print_app_sequence vars t1;
      print_space();
      print_tm_helper false vars t2;
      cbox ()
  | _ -> print_tm_helper false vars t

let spr = Buffer.add_string

(* Term -> string translation *)
let rec string_of_tm buf vars t =
  match t with
  | TmFun(fi, x, tyT1, t2) ->
      let rec pr_fun vars t =
            (match t with
             | TmFun(fi, x, tyT1, t2) ->
                 let (vars', x') = (pick_fresh_var vars x) in
                 (spr buf (" (" ^ x ^ ":");
                  string_of_tm buf vars tyT1;
                  spr buf ")" );
                 pr_fun vars' t2
             | _ ->
                 spr buf " => ";
                 string_of_tm buf vars t)
      in
      spr buf "(fn"; pr_fun vars t; spr buf ")"
  | TmLet(fi, x, t1, t2) ->
      (let (vars', x') = (pick_fresh_var vars x) in
      spr buf "(let ";
      spr buf (x' ^ " = "); string_of_tm buf vars t1;
      spr buf " in "; string_of_tm buf vars' t2; spr buf ")")
  | TmArrow(fi, x, tyS, tyT) ->
      let rec pr_arrow vars t =
        (match t with
        | TmArrow(fi, x, tyS, tyT) ->
            (let (vars', x') = (pick_fresh_var vars x) in
            (if not(x = "_") then (spr buf (x' ^ ":")));
            string_of_tm buf vars tyS;
            spr buf " -> ";
            pr_arrow vars' tyT);
        | _ -> string_of_tm buf vars t)
      in
      spr buf "(";
      pr_arrow vars t;
      spr buf ")";
  | TmApp(fi, t1, t2) ->
      spr buf "(";
      string_of_app_sequence buf vars t;
      spr buf ")";
  | TmVar(fi, x, check) ->
      if check_sane_vars fi vars x check  then
        spr buf (index_to_var fi vars x)
      else
        (pr ("[******************bad index: "
            ^ (sanity_check_string fi vars x check) ^ "]");
        spr buf "!")

  | TmPrimitive(fi, name, _) -> spr buf name

  | TmPlaceHolder(fi, PlaceHolder p, subst) ->

      let process_one_subst initial x tm =
        match tm with
          | TmVar (_,y,_) when x = y -> false
          | tm ->
              if initial then spr buf "[";
              string_of_tm buf vars tm;
              true
      in

      let rec print_subst initial index s =
        match s with
          | [] -> ();
          | [tm] ->
              let printed = process_one_subst initial index tm in
              if printed || (not initial) then
                spr buf "]";

          | tm :: tl ->
              let printed = process_one_subst initial index tm in
              if printed then spr buf ", ";
              print_subst (initial && (not printed)) (index+1) tl
      in

      spr buf "/*PH*/Dynamic"

(* This function assumes that parentheses have already been
   opened around a sequence of applications. *)
and string_of_app_sequence buf vars t =
  match t with
  | TmApp (fi,t1,t2) ->
      string_of_app_sequence buf vars t1;
      spr buf " ";
      string_of_tm buf vars t2;
  | _ -> string_of_tm buf vars t
(* End of term -> string translation *)

let print_tm vars t = print_tm_helper true vars t

let print_binding ctx b =
  let vars = var_list_from_ctx ctx in
  match b with
  | VarBind(tyT, None) -> pr ":"; print_tm vars tyT
  | VarBind(tyT, Some t) -> pr ":"; print_tm vars tyT;
      debug "valuebinding"
        (fun () -> print_space(); pr "="; print_space(); print_tm vars t);;

let rec print_ctx ctx =
   if Options.nopenv()
   then ()
   else
   (match ctx with
   | [] -> pr "0"
   | (s,bind) :: ctx' ->
   print_ctx ctx'; pr ","; force_newline();
   obox();
   pr s; print_binding ctx' bind;
   cbox())

let print_replacement r =
  let pr_p ctx p t = pr p; pr " := "; print_tm (var_list_from_ctx ctx) t in
  let rec print_rec initial r =
    match r with
      | [] -> pr "[]"
      | [ctx, PlaceHolder p, t] ->
          if initial then pr "["; pr_p ctx p t; pr "]"
      | (ctx, PlaceHolder p, t)::tl ->
          if initial then pr "["; pr_p ctx p t; pr ", "; print_rec false tl
  in
  print_rec true r

let print_str_tm str ctx tm =
  pr str;
  let vars = var_list_from_ctx ctx in
  print_tm vars tm;
  force_newline()

(* Printing subtype queries and responses *)


let print_subtype_judgement ctx s t =
  obox(); pr "(";
  print_ctx ctx;
  pr ") "; cbox();
  force_newline();
  pr "|- ";
  let vars = var_list_from_ctx ctx in
  obox(); print_tm vars s; cbox(); break();
  pr " <: ";
  obox(); print_tm vars t; cbox()
;;

let print_subtype_result ctx b =
  match b with
  | True -> pr " Yes ";
  | Maybe -> pr " Maybe ";
  | False None -> pr " No ";
  | False (Some tm) -> pr " No; counterexample: ";
      print_tm (var_list_from_ctx ctx) tm
;;

let print_subtype_test ctx s t b =
  print_subtype_judgement ctx s t;
  print_space ();
  print_subtype_result ctx b;
  force_newline ()
;;
