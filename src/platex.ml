open Primitives
open Syntax
open Print
open Prim_defs

let rec print_prim_app vars t =
  match prim_name t with
  | "Refine" -> (match get_app_args t with
      | [tyT; TmFun(_, x, tyT', pred)] ->
          let vars' = x::vars in
          print_string ("\\predty{"^x^"}{");
          latex_print_tm vars tyT' true;
          print_string "}{";
          latex_print_tm vars' pred true;
          print_string "}"
      | _ -> raise (Invalid_argument "print_prim_app"))
  | "cast" -> (match get_app_args t with
      | [tyS; tyT; v] ->
          print_string "\\cast{";
          latex_print_tm vars tyS true;
          print_string "}{";
          latex_print_tm vars tyT true;
          print_string "}{";
          latex_print_tm vars v true;
          print_string "}"
      | _ -> raise (Invalid_argument "print_prim_app"))
  | "add" -> (match get_app_args t with
      | [left; right] ->
          latex_print_tm vars left true;
          print_string " + ";
          latex_print_tm vars right true;
      | _ -> raise (Invalid_argument "print_prim_app"))
  | "sub" -> (match get_app_args t with
      | [left; right] ->
          latex_print_tm vars left true;
          print_string " - ";
          latex_print_tm vars right true;
      | _ -> raise (Invalid_argument "print_prim_app"))
  | "mul" -> (match get_app_args t with
      | [left; right] ->
          latex_print_tm vars left true;
          print_string " \\times ";
          latex_print_tm vars right true;
      | _ -> raise (Invalid_argument "print_prim_app"))
  | "geq" -> (match get_app_args t with
      | [left; right] ->
          latex_print_tm vars left true;
          print_string " \\geq ";
          latex_print_tm vars right true;
      | _ -> raise (Invalid_argument "print_prim_app"))
  | "leq" -> (match get_app_args t with
      | [left; right] ->
          latex_print_tm vars left true;
          print_string " \\leq ";
          latex_print_tm vars right true;
      | _ -> raise (Invalid_argument "print_prim_app"))
  | "iff" -> (match get_app_args t with
      | [left; right] ->
          latex_print_tm vars left true;
          print_string " \\Iff ";
          latex_print_tm vars right true;
      | _ -> raise (Invalid_argument "print_prim_app"))
  | "inteq" -> (match get_app_args t with
      | [left; right] ->
          latex_print_tm vars left true;
          print_string " = ";
          latex_print_tm vars right true;
      | _ -> raise (Invalid_argument "print_prim_app"))
  | _ -> latex_print_tm vars t false
and latex_print_tm vars t pretty_prims =
  match t with
  | TmVar(fi, x, check) ->
      if check_sane_vars fi vars x check then
        print_string (index_to_var fi vars x)
      else
        print_string "bad var"
  | TmFun(_, x, tyT, body) ->
      let vars' = x::vars in
      print_string ("\\lam{"^x^"}{");
      latex_print_tm vars tyT true;
      print_string "}{";
      latex_print_tm vars' body true;
      print_string "}"
  | TmApp(fi, t1, t2) ->
      if (is_full_prim_app fi (ctx_from_var_list vars) t) && pretty_prims then
        print_prim_app vars t
      else
      (print_string "(";
      latex_print_tm vars t1 true;
      print_string "~";
      latex_print_tm vars t2 true;
      print_string ")";)
  | TmArrow(_, x, tyS, tyT) ->
      let vars' = x::vars in
      print_string "(";
      if x = "_" then
        print_string "\\Funty{"
      else
        print_string ("\\Funtyd{"^x^"}{");
      latex_print_tm vars tyS true;
      print_string "}{";
      latex_print_tm vars' tyT true;
      print_string "})"
  | TmLet(_, x, t1, t2) ->
      let vars' = x::vars in
      print_string ("\\letexp{"^x^"}{");
      latex_print_tm vars t1 true;
      print_string "}{";
      latex_print_tm vars' t2 true;
      print_string "}"
  | TmPrimitive(_, name, _) ->
      print_string ("\\t{"^name^"}");
  | TmPlaceHolder(_, _, _) -> print_string "?"

let main() =
  let str = read_line () in
  (* Bind common metavariables *)
  let vars = ["S"; "T"; "U"; "V"; "u"; "v"; "x"; "y"; "z"] in
  let t = parse_term_string vars str in
  latex_print_tm vars t true;
  print_string "\n"

let _ = main()
