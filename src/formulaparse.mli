type var = string
type number = string
type sTerm = 
    SVar of var 
  | SInt of number
  | SFun of var * sTerm list
  | SAdd of sTerm * sTerm
  | SSub of sTerm * sTerm
  | SMul of sTerm * sTerm

type sPred =
    SAnd of sPred * sPred
  | SOr of sPred * sPred
  | SNot of sPred
  | SImp of sPred * sPred
  | SIff of sPred * sPred
(*  | SForAll of var list * sPred*)
  | SEq of sTerm * sTerm
  | SNeq of sTerm * sTerm
  | SLt of sTerm * sTerm
  | SLeq of sTerm * sTerm
  | SGt of sTerm * sTerm
  | SGeq of sTerm * sTerm
  | STrue
  | SFalse
  | SPropVar of var
(*  | SAndL of (sPred list)*)

val s_and: string -> string -> string
val s_imp: string -> string -> string

val lift_sterm: string -> string

val bool_term_rules: string

val parse_spred:  sPred -> string
val parse_sterm: sTerm -> string
