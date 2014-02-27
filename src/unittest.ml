open Format
open Support.Error
open Support.Pervasive
open Syntax
open Print
open Primitives
open Prim_defs
open Compile
open Eval
open Subtype
open Basicalg
open Options

let () = assert (is_full_prim_app dummyinfo [] (tm_prim "true"));;

let di = dummyinfo;;

let ty_Int = tm_prim "Int";;
let ty_Bool = tm_prim "Bool";;
let ty_Unit = tm_prim "Unit";;
let tm_unit = tm_prim "unit";;
let tm_true = tm_prim "true";;

(* Yes, this is a weird thing to do (because of the weird index in the
 * variable). But it may help find bugs. *)
let tm = subst_top_tm_in_tm tm_true
         (TmFun(di,
                "x",
                ty_Bool,
                TmVar(di,
                      0,
                      sanity_check_from_var_list ["?"; "x"])))
in assert (term_eq tm (TmFun(di,
                             "x",
                             ty_Bool,
                             TmVar(di,
                                   0,
                                   sanity_check_from_var_list ["x"]))));;
let tm = subst_top_tm_in_tm tm_true (TmVar(di,
                                           0,
                                           sanity_check_from_var_list ["x"]))
  in
  assert (term_eq tm tm_true);;

let tm = make_prim_app_sequence di [] "cast" [string_of_info di]
           [ty_Bool; ty_Bool; tm_true] in
let tm' = make_cast di [] ty_Bool ty_Bool tm_true in
  assert (term_eq tm tm');;

assert (is_full_prim_app di [] (make_cast di [] ty_Bool ty_Bool tm_true));;

let ty_star = primitive_ty di [] "*" [];;
let ty_Unit = primitive_ty di [] "Unit" [];;
let ty_unit = primitive_ty di [] "unit" [];;
let ty_Bool = primitive_ty di [] "Bool" [];;
let ty_true = primitive_ty di [] "true" [];;
let ty_false = primitive_ty di [] "false" [];;
let ty_not = primitive_ty di [] "not" [];;
let ty_Int = primitive_ty di [] "Int" [];;
let ty_add = primitive_ty di [] "add" [];;
let ty_sub = primitive_ty di [] "sub" [];;
let ty_IF = primitive_ty di [] "IF" [];;
let ty_cast = primitive_ty di [] "cast" [];;
let ty_Refine = primitive_ty di [] "Refine" [];;
let ty_fix = primitive_ty di [] "fix" [];;

let di = dummyinfo in
let intTy = tm_prim "Int" in
let pos =
  make_prim_app_sequence di [] "gt" []
    [(TmVar(di, 0, sanity_check_from_var_list ["?"]));(tm_prim "0")] in
let refinement = make_refinement di [] "x" intTy pos in
  assert(is_full_prim_app di [] refinement)
