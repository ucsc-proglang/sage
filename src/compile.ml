open Format
open Support.Error
open Support.Pervasive
open Syntax
open Subtype
open Primitives
open Prim_defs
open Eval
open Print
open Options

(* ---------------------------------------------------------------------- *)
(* Subtype wrapper for counting *)


let num_no = ref 0
let num_maybe = ref 0
let num_yes = ref 0
let num_id = ref 0
let toplevel = ref true

let print_stats() =
  pr(Printf.sprintf "subtype YES/IDENTICAL/MAYBE/NO:   %d/%d/%d/%d"
       !num_yes !num_id !num_maybe !num_no);
  force_newline()

let () = Options.at_exit print_stats

let check fi ctx tm tyS tyT =
  if !toplevel && term_eq tyS tyT then num_id := !num_id + 1;
  let oldtoplevel = !toplevel in
  toplevel := false;
  let result = is_subtype fi ctx tyS tyT in
  toplevel := oldtoplevel;
  match result with
  | True ->
      if !toplevel then num_yes := !num_yes + 1;
      tm

  | Maybe ->
      if Options.perfect()
      then warnf_at fi
        (fun () ->
           print_str_tm "The term " ctx tm;
           print_str_tm "has type " ctx tyS;
           print_str_tm "may not be a subtype of expected type " ctx tyT);
      (if !toplevel then num_maybe := !num_maybe + 1);
      make_cast fi ctx tyS tyT tm

  | False _ ->
      (if !toplevel then num_no := !num_no + 1);
      force_newline();
      warnf_at fi
        (fun () ->
           print_str_tm "The term " ctx tm;
           print_str_tm "has type " ctx tyS;
           print_str_tm "which is not a subtype of expected type " ctx tyT);
      tm

 (* ---------------------------------------------------------------------- *)
 (* Join of types *)

let rec join_type ctx s t =
  match (s,t) with
  | _ when term_eq s t -> s
  | (TmApp(fi, TmApp(_, TmPrimitive(_,"Refine",[]), tyS'), fs),
     TmApp(_, TmApp(_, TmPrimitive(_,"Refine",[]), tyT'), ft)) ->
       let t = join_type ctx tyS' tyT' in
       let scheck =
         add_checked_var (sanity_check_from_var_list
                            (var_list_from_ctx ctx))
           "dummy"
       in
       let f = TmFun(fi,"dummy", tm_prim ~fi "Dynamic",
                     TmApp(fi, TmApp(fi, TmPrimitive(fi,"or",[]),
                                     TmApp(fi, fs, TmVar(fi, 0, scheck))),
                           TmApp(fi, ft, TmVar(fi, 0, scheck))))
       in
       TmApp(fi, TmApp(fi, TmPrimitive(fi,"Refine",[]), t), f)
  | _ ->
      pr "DYNAMIC type for IF\n";
      tm_prim "Dynamic"
;;

let rec strip_refine_and_eval fi ctx ty =
  match ty with
  | TmApp(_, TmApp(_, TmPrimitive(_,"Refine",[]), ty'), t) ->
      strip_refine_and_eval fi ctx ty'
  | _ ->
        let r = eval_type ctx ty in
        if term_eq r ty
        then r
        else strip_refine_and_eval fi ctx r
;;

 (* ---------------------------------------------------------------------- *)
 (* Compilation *)


let unexpected_term ctx tm s =
  pr "\nUnexpected term:\n";
  print_tm (var_list_from_ctx ctx) tm;
  force_newline();
  raise (NoRuleApplies s)

let rec compile_tm ctx tm =
  debug_wrapper "compile"
    (fun () -> print_str_tm "compile " ctx tm)
    (fun (r,ty) ->
      print_str_tm "compile-result " ctx r;
      print_str_tm "compile-type " ctx ty)
    (fun() ->
  match tm with
  | TmVar(fi, i, n) ->
      (match get_binding fi ctx i with
           | VarBind(ty, _) ->
           let vars = var_list_from_ctx ctx in
           let _, x = pick_fresh_var vars "svar" in
           let scheck = add_checked_var
             (sanity_check_from_var_list vars)
             x
           in
           let new_ty = TmApp(fi, TmApp(fi, tm_prim "Refine", ty),
                              TmFun(fi, x, ty,
                                    TmApp(fi,
                                          TmApp(fi,
                                                tm_prim "eq",
                                                TmVar(fi, 0, scheck)),
                                          TmVar(fi, i + 1, scheck))))
           in
           (tm, (replace_info new_ty fi))
      )
  | TmPrimitive(fi, name, labels) ->
      (tm, primitive_ty fi ctx name labels)
  | TmFun(fi, x, tyS, tm) ->
      let tyS' = compile_check fi ctx tyS (tm_prim ~fi "*") in
      let ctx' = (x,(VarBind(tyS', None)))::ctx in
      let (tm', tyT) = compile_tm ctx' tm in
          (TmFun(fi, x, tyS', tm'), TmArrow(fi, x, tyS', tyT))
  | TmLet(fi, x, s, t) ->
      let (s', tyS') = compile_tm ctx s in
      let s2 = (if is_val fi ctx s' then Some(s') else None) in
      let ctx' = (x,(VarBind(tyS', s2)))::ctx in
      let (t', tyT') = compile_tm ctx' t in
          (TmLet(fi, x, s', t'), subst_top_tm_in_tm s' tyT')
  | TmArrow(fi, x, tyS, tyT) ->
      let tyS' = compile_check fi ctx tyS (tm_prim ~fi "*") in
      let ctx' =  (x,(VarBind(tyS', None)))::ctx in
      let tyT' = compile_check fi ctx' tyT (tm_prim ~fi "*") in
          (TmArrow(fi, x, tyS', tyT'), tm_prim ~fi "*")

  | TmApp(fi, tm1, tm2) -> (
      let (tm1', ty1) = compile_tm ctx tm1 in
      match strip_refine_and_eval fi ctx ty1 with
      | TmArrow(fi', x, tyS, tyT) ->
          let tm2' = compile_check fi ctx tm2 tyS in
          (TmApp(fi, tm1', tm2'), subst_top_tm_in_tm tm2' tyT)
      | ty1' ->
          force_newline();
          print_string "Note: ";
          print_info fi;
          print_str_tm " The function " ctx tm1;
          print_str_tm "has type " ctx ty1';
          pr "which may not be a function type";
          force_newline();

          let dynamic = tm_prim ~fi "Dynamic" in
          let dynamic_fn = TmArrow(fi, "_",  dynamic, dynamic) in
          let tm1'' = check fi ctx tm1' ty1' dynamic_fn in
          let tm2' = compile_check fi ctx tm2 dynamic in
          (TmApp(fi, tm1'', tm2'), dynamic)
      )
  | TmPlaceHolder(fi, ph, subs) ->
      (* placeholders treated as Dynamic *)
      compile_tm ctx (tm_prim ~fi "Dynamic")
  )
and compile_check fi ctx tm tyT =
  match tyT with
  | TmArrow(fiArrow, x, tyS', tyT') ->
      (match tm with
      | TmFun(fi', y, (TmPlaceHolder(_, _, _)
      | TmPrimitive(_,"Dynamic",_)), tm') ->
          let (ctx',x') = bind_fresh_name ctx y (VarBind(tyS', None)) in
          let tm'' = compile_check fi ctx' tm' tyT' in
          TmFun(fi', x', tyS', tm'')
      | _ ->
          let (tm', tyS) = compile_tm ctx tm in
          check fi ctx tm' tyS tyT)
  | _ ->
      let (tm', tyS) = compile_tm ctx tm in
      check fi ctx tm' tyS tyT

let () = Eval.eval_compiler := compile_tm

let typeof ctx t =
  debug_wrapper "compile"
    (fun () -> pr "compile.typeof\n")
    (fun ty -> pr "compile.typeof\n")
    (fun () ->
      let (t', ty) = compile_tm ctx t in
      ty)
