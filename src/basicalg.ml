open Support.Error
open Support.Pervasive
open Syntax
open Primitives
open Eval
open Subtype
open Format
open Print
open Simplify
open Sformula
open Options

(* This file MUST be kept consistent with the subtyping rules in the
 * documentation. *)

let is_base_type t =
  match t with
  | TmPrimitive(_,prim,_) -> List.mem prim ["Bool"; "Unit"; "Int"; "*" ]
  | TmArrow(_,_,_,_) -> true
  | _ -> false

(* The core subtyping algorithm - uses subtype for recursive calls *)

let rec core_subtype (subtype : subtype_tester) fi ctx tyS tyT =
  (* Via-Eval-L and Via-Eval-R *)
  try subtype fi ctx (eval_type_many (Options.basicsteps()) ctx tyS ) tyT
  with NoEvalRule _ ->
    try subtype fi ctx tyS (eval_type_many (Options.basicsteps()) ctx tyT)
    with NoEvalRule _ ->
      let pred_holds_on_type fi ctx term ty =
        match (eval_type ctx term) with
        | TmFun(_,x,_,t) ->
            (
              match ctx with
              | (_,VarBind( TmApp(_, TmApp(_, TmPrimitive(_, "Refine", _),
                                           TmPrimitive(_, "Unit", _)),
                                  TmFun(_,_,_, body)),
                            _))::r
                  when term_eq body t
                    -> True
              | _ ->
                  let ctx' = add_binding ctx x (VarBind(ty,None)) in
                  (prove ctx' t)
            )
        | _ -> Maybe
      in

      match (tyS,tyT) with

      | (_,_) when term_eq tyS tyT  ->
          True (* Via-Id *)

      | (_,TmPrimitive(_, "Dynamic", [])) ->
          True (* Via-Dyn-R *)

      | (TmPrimitive(_, "Dynamic", []),_) ->
          Maybe (* Via-Dyn-L *)

      | (TmArrow(_,x,tyS1,tyS2), TmArrow(_,_,tyT1,tyT2)) ->
          let ctx' = (add_binding ctx x (VarBind(tyT1,None))) in

          (* which to do first? who knows. guess. *)

          (match subtype fi ctx tyT1 tyS1, subtype fi ctx' tyS2 tyT2 with
           | True,    True    -> True
           | False x, _
           | _,       False x -> False x (* no rule *)
           | _,       _       -> Maybe )

      | (_, TmApp(_, TmApp(_, TmPrimitive(_,"Refine",[]), tyT'), _))
          when (Options.norefine()) -> subtype fi ctx tyS tyT'

      | (TmApp(_, TmApp(_, TmPrimitive(_,"Refine",[]), tyS'), _), _)
          when (Options.norefine()) -> subtype fi ctx tyS' tyT

      | (_, TmApp(_, TmApp(_, TmPrimitive(_,"Refine",[]), tyT'), t)) ->
          debug_wrapper "basicalg"
            (fun () -> pr "As-Ref-R: "; print_subtype_judgement ctx tyS tyT)
            (fun res -> pr "As-Ref-R: "; print_subtype_test ctx tyS tyT res)
            (fun () ->
               match subtype fi ctx tyS tyT' with
               | Maybe -> Maybe
               | False x -> False x
               | True  -> pred_holds_on_type fi ctx t tyS)

      (* As-Ref-L *)
      | (TmApp(_, TmApp(_, TmPrimitive(_,"Refine",[]), tyS'), _), _) ->
          subtype fi ctx tyS' tyT

      | (_,TmApp(_,_,_))
      | (TmApp(_,_,_),_)
      | (_,TmVar(_,_,_))
      | (TmVar(_,_,_),_)
      | (TmLet(_,_,_,_),_)
      | (_,TmLet(_,_,_,_)) -> Maybe

      | _ -> False None

(* A wrapper for printing results of subtype algorithm *)

let print_sub str ctx tyS tyT result =
  let maybe = Options.pmaybe() && (result = Maybe) in
  if (maybe || Options.psub()) then (
    pr str;
    print_subtype_test ctx tyS tyT result;
    force_newline();
  );
  result

(* The subtype algorithm that takes care of termination (via n), and also
   identifies looping in the algorithm due to fix, and takes care of it.
*)

let rec subtype2 subtype_delegate (stack:(term*term*bool ref) list)
                                  (n:int) fi ctx tyS tyT =
  debug_wrapper "basicalg"
    (fun () -> pr "subtype2 "; print_subtype_judgement ctx tyS tyT;
       pr (if term_eq tyS tyT then "EQ" else "NEQ"))
    (fun res -> pr "subtype2 "; print_subtype_test ctx tyS tyT res)
    (fun () ->
       if n = 0 then (
         pr "Timeout!!!!!!!!!";
         force_newline();
         Maybe
       )
       else
         try
           subtype_delegate fi ctx tyS tyT
         with Db.NotFoundInDB ->
           try
             let (_,_,r) =
               List.find (fun (ty1,ty2,r) ->
                            term_eq ty1 tyS && term_eq ty2 tyT)
                 stack
             in
             pr "Recursion in subtyping: ";
             let vars = var_list_from_ctx ctx in
             print_tm vars tyS;
             pr " <: ";
             print_tm vars tyT;
             force_newline();
             r := true;
             True
           with Not_found ->
                 let r = ref false in
                 let rec_subtype fi ctx tyS' tyT' =
                   subtype2
                 subtype_delegate
                 ((tyS,tyT,r)::stack)
                 (n - 1)
                 fi ctx tyS' tyT' in
                 core_subtype rec_subtype fi ctx tyS tyT
    )

(* The external interface *)

let basic_subtype subtype_delegate fi ctx tyS tyT =
  print_sub "subtype-top: " ctx tyS tyT
    (subtype2 subtype_delegate [] (Options.maxeval()) fi ctx tyS tyT)
