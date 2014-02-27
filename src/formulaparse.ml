open Support.Error
open Support.Pervasive
open Syntax
open Support
open Print
open Options
open Format

type tName = string
type pName = string
type var = string
type number = string

type sTerm =
  | SVar of var
  | SInt of number
  | SFun of tName * (sTerm list)
  | SAdd of sTerm * sTerm
  | SSub of sTerm * sTerm
  | SMul of sTerm * sTerm

type sPred =
  | SAnd of sPred * sPred
  | SOr of sPred * sPred
  | SNot of sPred
  | SImp of sPred * sPred
  | SIff of sPred * sPred
(*  | SForAll of (var list) * sPred*)
(* simplify Literals *)
  | SEq of sTerm * sTerm
  | SNeq of sTerm * sTerm
  | SLt of sTerm * sTerm
  | SLeq of sTerm * sTerm
  | SGt of sTerm * sTerm
  | SGeq of sTerm * sTerm
  | STrue
  | SFalse
  | SPropVar of pName
(*  | SAndL of (sPred list)*)


(*------- STRING CREATION FUNCTIONS -------*)
let unary_s command w1 = ("("^command^" "^w1^")")
let binary_s command w1 w2 = ("("^command^" "^w1^" "^w2^")")

let s_add w1 w2 = binary_s "+" w1 w2
let s_and w1 w2 = binary_s "AND" w1 w2
let s_eq w1 w2 = binary_s "EQ" w1 w2
let s_neq w1 w2 = binary_s "NEQ" w1 w2
let s_forall args w2 = binary_s "FORALL" args w2
let s_geq w1 w2 = binary_s ">=" w1 w2
let s_gt w1 w2 = binary_s ">" w1 w2
let s_iff w1 w2 = binary_s "IFF" w1 w2
let s_imp w1 w2 = binary_s "IMPLIES" w1 w2
let s_leq w1 w2 = binary_s "<=" w1 w2
let s_lt w1 w2 = binary_s "<" w1 w2
let s_mul w1 w2 = binary_s "*" w1 w2
let s_not w1 = unary_s "NOT" w1
let s_num w1 = string_of_int w1
let s_or w1 w2 =  binary_s "OR" w1 w2
let s_sub w1 w2 = binary_s "-" w1 w2
let s_true ()= "TRUE"
let s_false () = "FALSE"

let b_true () = "boolTrue"
let b_false () = "boolFalse"

let s_boolAnd w1 w2 = binary_s "boolAnd" w1 w2
let s_boolOr w1 w2 = binary_s "boolOr" w1 w2
let s_boolNot w1 = unary_s "boolNot" w1
let s_boolEq w1 w2 = binary_s "boolEq" w1 w2
let s_boolImp w1 w2 = binary_s "boolImp" w1 w2
let s_intEq w1 w2 = binary_s "intEq" w1 w2
let s_intGeq w1 w2 = binary_s "intGeq" w1 w2
let s_intLeq w1 w2 = binary_s "intLeq" w1 w2
let s_intGt w1 w2 = binary_s "intGt" w1 w2
let s_intLt w1 w2 = binary_s "intLt" w1 w2


(* DEFAULT GLOBAL PREDICATE - Define boolean terms *)
let bool_term_rules =
  (* boolAnd *)
  ("(BG_PUSH (AND "^
   (s_forall "(X Y)" (s_iff (s_eq "(boolAnd X Y)" (b_true()))
       (s_and (s_eq "X" (b_true()))(s_eq "Y" (b_true())))))
   ^" "^
   (s_forall "(X Y)" (s_iff (s_eq "(boolAnd X Y)" (b_false()))
       (s_or (s_eq "X" (b_false()))(s_eq "Y" (b_false())))))
   ^" "^
   (* boolOr *)
   (s_forall "(X Y)" (s_iff (s_eq "(boolOr X Y)" (b_true()))
       (s_or (s_eq "X" (b_true()))(s_eq "Y" (b_true())))))
   ^" "^
   (s_forall "(X Y)" (s_iff (s_eq "(boolOr X Y)" (b_false()))
       (s_and (s_eq "X" (b_false()))(s_eq "Y" (b_false())))))
   ^" "^
   (* boolNot *)
   (s_forall "(X)" (s_iff (s_eq "(boolNot X)" (b_true()))
                      (s_eq "X" (b_false()))))
   ^" "^
   (s_forall "(X)" (s_iff (s_eq "(boolNot X)" (b_false()))
       (s_eq "X" (b_true()))))
   ^" "^
   (* boolEq *)
   (s_forall "(X Y)" (s_iff (s_eq "(boolEq X Y)" (b_true()))
       (s_or (s_and (s_eq "X" (b_true())) (s_eq "Y" (b_true())))
       (s_and (s_eq "X" (b_false())) (s_eq "Y" (b_false()))) )))
   ^" "^
   (s_forall "(X Y)" (s_iff (s_eq "(boolEq X Y)" (b_false()))
       (s_or (s_and (s_eq "X" (b_false())) (s_eq "Y" (b_true())))
       (s_and (s_eq "X" (b_false())) (s_eq "Y" (b_true()))) )))
     ^" "^
   (* boolImp *)
   (s_forall "(X Y)" (s_iff (s_eq "(boolImp X Y)" (b_true()))
       (s_or (s_eq "X" (b_false())) (s_eq "Y" (b_true())))))
   ^" "^
   (s_forall "(X Y)" (s_iff (s_eq "(boolImp X Y)" (b_false()))
       (s_and (s_eq "X" (b_true())) (s_eq "Y" (b_false())))))
   ^" "^
   (* intEq *)
   (s_forall "(X Y)" (s_iff (s_eq "(intEq X Y)" (b_true())) (s_eq "X" "Y")))
   ^" "^
   (s_forall "(X Y)" (s_iff (s_eq "(intEq X Y)" (b_false())) (s_neq "X" "Y")))
   ^" "^
   (* intGeq *)
   (s_forall "(X Y)" (s_iff (s_eq "(intGeq X Y)" (b_true())) (s_geq "X" "Y")))
   ^" "^
   (s_forall "(X Y)" (s_iff (s_eq "(intGeq X Y)" (b_false())) (s_lt "X" "Y")))
   ^" "^
   (* intLeq *)
   (s_forall "(X Y)" (s_iff (s_eq "(intLeq X Y)" (b_true())) (s_leq "X" "Y")))
   ^" "^
   (s_forall "(X Y)" (s_iff (s_eq "(intLeq X Y)" (b_false())) (s_gt "X" "Y")))
   ^" "^
   (* intGt *)
   (s_forall "(X Y)" (s_iff (s_eq "(intGt X Y)" (b_true())) (s_gt "X" "Y")))
   ^" "^
   (s_forall "(X Y)" (s_iff (s_eq "(intGt X Y)" (b_false())) (s_leq "X" "Y")))
   ^" "^
   (* intLt *)
   (s_forall "(X Y)" (s_iff (s_eq "(intLt X Y)" (b_true())) (s_lt "X" "Y")))
   ^" "^
   (s_forall "(X Y)" (s_iff (s_eq "(intLt X Y)" (b_false())) (s_geq "X" "Y")))
   ^"))")

let lift_sterm (sterm:string) : string =
  s_eq sterm (b_true())

let expand_string_list (s_list:(string list)) =
  List.fold_right
    (fun (s1:string) (s2:string) ->
      if (s2 = "")
      then s1
      else s1^" "^s2)
    s_list ""

let rec parse_sterm (term:sTerm) =
  match term with
    SVar var -> var
  | SInt num -> num
  | SFun (var,args) ->
      let string_args = List.map parse_sterm args in
      "("^var^" "^(expand_string_list string_args)^")"
  | SAdd(t1,t2) -> s_add (parse_sterm t1) (parse_sterm t2)
  | SSub(t1,t2) -> s_sub (parse_sterm t1) (parse_sterm t2)
  | SMul(t1,t2) -> s_mul (parse_sterm t1) (parse_sterm t2)

(*let rec help_flatten_sand (formula:sPred) :(sPred list)=
  match formula with
    SAnd(t1,t2) ->
      let s1 = help_flatten_sand t1 in
      let s2 = help_flatten_sand t2 in
      List.append s1 s2
  |_ -> formula::[]

let rec flatten_sand (formula:sPred) : string =
  parse_spred (SAndL(help_flatten_sand formula))*)

let rec parse_spred (formula:sPred) :string =
  match formula with
    SAnd(t1,t2) -> s_boolAnd (parse_spred t1) (parse_spred t2)
  | SOr(t1,t2) -> s_boolOr (parse_spred t1) (parse_spred t2)
  | SImp(t1,t2) -> s_boolImp (parse_spred t1) (parse_spred t2)
  | SNot t -> s_boolNot (parse_spred t)
  | SIff(t1,t2) -> s_boolEq (parse_spred t1) (parse_spred t2)
(*  | SForAll(vars,t) -> s_forall (expand_string_list vars) (parse_spred t)*)
  | SEq(t1,t2) -> s_intEq (parse_sterm t1) (parse_sterm t2)
  | SNeq(t1,t2) -> s_boolNot (s_intEq (parse_sterm t1) (parse_sterm t2))
  | SLt(t1,t2) -> s_intLt (parse_sterm t1) (parse_sterm t2)
  | SLeq(t1,t2) -> s_intLeq (parse_sterm t1) (parse_sterm t2)
  | SGt(t1,t2) -> s_intGt (parse_sterm t1) (parse_sterm t2)
  | SGeq(t1,t2) -> s_intGeq (parse_sterm t1) (parse_sterm t2)
  | STrue -> b_true()
  | SFalse -> b_false()
  | SPropVar var -> var
(*  | SAndL plist ->
      let slist = List.map parse_spred plist in
      "(AND "^(expand_string_list slist)^")"*)
