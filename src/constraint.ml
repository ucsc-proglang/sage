open Support.Pervasive
open Support.Error
open Syntax
open Primitives
open Format
open Print
open Term

type constraint_t = info * context * term * term

let star fi = TmPrimitive(fi, "*", [])

let rec type_constraints ctx t =
  match t with
    | TmPlaceHolder (fi, name, subst) -> star fi, []

    | TmVar (fi, index, ctx_size) -> (get_ty_from_ctx fi ctx index), []

    | TmPrimitive (fi, name, labels) -> primitive_ty fi ctx name labels, []

    | TmFun (fi, var, ty, body) ->
        let k_ty,   d_ty   = type_constraints ctx ty in
        let t_body, d_body = type_constraints (bind_ty ctx var ty) body in
        let ty_constraint = fi, ctx, k_ty, star fi in
        (TmArrow (fi, var, ty, t_body)),
        (ty_constraint :: (d_ty @ d_body))

    | TmArrow (fi, var, arg, result) ->
        let k_arg, d_arg = type_constraints ctx arg in
        let k_res, d_res = type_constraints (bind_ty ctx var arg) result in
        star fi,
        [fi, ctx, k_arg, star fi;
          fi, ctx, k_res, star fi]
        @ d_arg @ d_res
(*
    | TmApp (fi, t_fun, t_arg) -> error fi "Cannot gen constraints for apps"
  *)

   | TmApp (fi, t_fun, t_arg) ->
   let (ty_fn, d_fun) = type_constraints ctx t_fun in
   let (ty_arg, d_arg) = type_constraints ctx t_arg in
   let vars = var_list_from_ctx ctx in
   let alpha = fresh_placeholder fi vars in
   let beta = fresh_placeholder fi vars in
   let alphabeta = TmArrow(fi, "apparg", alpha, beta) in

   subst_top_tm_in_tm t_arg beta,

   [fi, ctx, ty_fn, alphabeta;
   fi, ctx, ty_arg, alpha]
   @ d_fun @ d_arg

    | TmLet (fi,x,v,body) ->
        let ty_v, d_v  = type_constraints ctx v in
        let ctx' = bind_ty ctx x ty_v in
        let ty_body, d_body = type_constraints ctx' body in

        let k_ty_v, d_ty_v = type_constraints ctx ty_v in
        let k_ty_body, d_ty_body = type_constraints ctx ty_body in

        subst_top_tm_in_tm v ty_body,
        [fi, ctx, k_ty_v, star fi;
         fi, ctx, k_ty_body, star fi]
        @ d_v @ d_body @ d_ty_v @ d_ty_body

let replace_in_tm r tm =
  let rec replace_one t (ctx,p,s as r) =
    match t with
      | TmPlaceHolder (fi,p',sub) when p' = p -> apply_delayed_subst sub s

      | TmPlaceHolder _
      | TmPrimitive _
      | TmVar _ -> t

      | TmLet(fi, var, t1, t2) ->
          TmLet(fi, var, replace_one t1 r, replace_one t2 r)
      | TmFun (fi, var, ty, body) ->
          TmFun (fi, var, replace_one ty r, replace_one body r)
      | TmArrow (fi, var, arg, res) ->
          TmArrow (fi, var, replace_one arg r, replace_one res r)
      | TmApp (fi, f, arg) ->
          TmApp (fi, replace_one f r, replace_one arg r)
  in
  List.fold_left replace_one tm r

let replace_in_binding r bind =
  match bind with
    | VarBind(ty, None) -> VarBind(replace_in_tm r ty, None)
    | VarBind(ty, Some(tm)) ->
        VarBind(replace_in_tm r ty, Some(replace_in_tm r tm))

let replace_in_constraint replacement (fi,ctx,t1,t2) =
  let ctx' = List.map (fun (name,bind) ->
                         name, replace_in_binding replacement bind) ctx in
    (*
  let (t1',_) = Eval.eval_to_whnf (Options.maxwhnf()) ctx
              (replace_in_tm replacement t1)
  in
  let (t2',_) = Eval.eval_to_whnf (Options.maxwhnf()) ctx
              (replace_in_tm replacement t2)
  in
     *)
    (* FIXME: this needs to be updated with new eval functions *)
  (fi, ctx, t1, t2)

let print_constraint (fi, ctx, subty, supty) =
  let vars = var_list_from_ctx ctx in
  print_ctx ctx;
  pr " |- ";
  print_tm vars subty;
  pr " <: ";
  print_tm vars supty

let greedy_unify (constraints : constraint_t list) : replacement =
  let unify_one ((fi,ctx,t1,t2) as con) =

    (*pr "Unifying: "; print_constraint con; force_newline ();*)

    (* Assuming already eval'd to WHNF *)
    if t1 = t2 then [], []
    else match t1,t2 with

      (* When the substitution is non-empty, we are looking at higher-order
       * unification which is quite undecidable, on top of our evaluation
       * to normal forms being undecidable. But then, we know how to handle
       * that :) *)
      | TmPlaceHolder (_,p,_), _ when not (has_delayed_subst t1)
          -> [ctx,p,t2], []

      | _, TmPlaceHolder (_,p,_) when not (has_delayed_subst t2)
          -> [ctx,p,t1], []


      | TmVar (fi1,i1,n1), TmVar(_,i2,n2) ->
          if i1 = i2 then [], []
          else error fi1 "Untypable - variables don't match in unification"

      | TmFun(_,var1,ty1,body1), TmFun(_,var2,ty2,body2) ->

          (* var1 and var2 are equal by virtue of Debruijn, but the type
             should actually be the intersection of ty1 and ty2 *)
          [], [fi, ctx, ty2, ty1;
               fi, bind_ty ctx var1 ty1, body1, body2]

      | TmArrow(_,var1,ty1,res1), TmArrow(_,var2,ty2,res2) ->

          (* var1 and var2 are equal by virtue of Debruijn, but the type
             should actually be the intersection of ty1 and ty2 *)
          [], [fi, ctx, ty1, ty2;
               fi, bind_ty ctx var1 ty1, res1, res2]

      | TmPrimitive(fi1, c1, _), TmPrimitive(fi2, c2, _) ->
          (* TODO: make my own exception to raise? *)
          if c1 = c2 then [], []
          else error fi1 "Untypable - constants don't match in unification"

      (* Wrong, but greedy - FIXME to be right but greedy *)
      | TmApp (_,s1,t1), TmApp (_,s2,t2) ->
          (* The variable name in the annotations might be different, but the
             debruijn should be right. *)
          [], [fi, ctx, s2, s1;
               fi, bind_ty ctx "_" s2, t1, t2]

      | t1, t2 ->
          pr "When trying to unify: ";
          print_tm (var_list_from_ctx ctx) t1;
          pr " <: ";
          print_tm (var_list_from_ctx ctx) t2;
          force_newline ();
          err "Incompatible WHNF during unification"
  in

  let rec unify constraints =
    match constraints with
      | [] -> []
      | hd :: tl ->
          let hd_unifier, added_constraints = unify_one hd in
          let tl' = List.map (replace_in_constraint hd_unifier) tl in
          let tl_unifier = unify (added_constraints @ tl') in
          hd_unifier @ tl_unifier
  in
  (* Sort the constraints to ensure that those without
     delayed substitutions come first.  This could be maintained as an
     invariant of the constraint generation but I think that would be more
     brittle. *)
  unify (List.sort
           (fun (_,_,s1,t1) (_,_,s2,t2) ->
              match
                (has_delayed_subst s1 || has_delayed_subst t1,
                 has_delayed_subst s2 || has_delayed_subst t2)
              with
                | true, false -> 1
                | false, true -> -1
                | _, _ -> 0)
           constraints)
    
