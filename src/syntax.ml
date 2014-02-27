open Format
open Support.Error
open Support.Pervasive
open List

(* ---------------------------------------------------------------------- *)
(* Datatypes *)

type label = string
type variable = string
type var_list = variable list

type placeholder = PlaceHolder of string
type delayed_substitution = term list
and term =
  | TmVar of info * int * sanity_check
  | TmFun of info * variable * ty * term
  | TmLet of info * variable * term * term
  | TmArrow of info * variable * ty * ty
  | TmApp of info * term * term
  | TmPrimitive of info * string * label list
  | TmPlaceHolder of info * placeholder * delayed_substitution
and ty = term
and value = term
and sanity_check = var_list

type binding =
  | VarBind of ty * (value option)

type context = (variable * binding) list
type replacement = (context * placeholder * term) list

type prim_info = {
  ty: ty;
  is_full_app_value: bool;
  eval_fn: (info ->  context -> label list -> term list -> term);
  cast_fn:
    (info -> context -> label list -> term list -> term -> value -> term)
}

type command =
  | Eval of info * term
  | Define of info * variable * term
  | Assume of context * term * term * bool
  | Check of context * term * term * bool
  | Prove of context * term * bool

exception NoRuleApplies of string

(* ---------------------------------------------------------------------- *)
(* Context management *)

let empty_ctx = []

let add_binding ctx x bind = (x, bind)::ctx

let bind_ty ctx var ty =
  add_binding ctx var (VarBind(ty, None))

let var_list_from_ctx ctx = map (fun (x, _) -> x) ctx

(* These contexts aren't really usable, but sometimes we have to fake them *)
let ctx_from_var_list vars =
  map (fun x -> (x, VarBind(TmPrimitive(dummyinfo, "*", []), None))) vars

let rec name_to_index fi ctx x =
  match ctx with
    | [] -> error fi ("Identifier " ^ x ^ " is unbound (1)")
    | (y, _)::rest ->
        if y=x then 0
        else 1 + (name_to_index fi rest x)


let rec is_name_bound vars x =
  match vars with
    | [] -> false
    | (y, _)::rest ->
        if y = x then true
        else is_name_bound rest x

let rec bind_fresh_name ctx x bind =
  if is_name_bound ctx x then bind_fresh_name ctx (x^"'") bind
  else ((x, bind)::ctx), x

(* Variable list management *)
let index_to_var fi vars x =
  try
    let xn = List.nth vars x in
    xn
  with Failure _ ->
    let msg =
      Printf.sprintf "Variable lookup failure: offset: %d, vars size: %d" in
    error fi (msg x (List.length vars))

let rec is_var_bound vars x =
  match vars with
    | [] -> false
    | y::rest ->
        if y = x then true
        else is_var_bound rest x

let rec var_to_index fi vars x =
  match vars with
    | [] -> error fi ("Identifier " ^ x ^ " is unbound (2)")
    | y::rest ->
        if y=x then 0
        else 1 + (var_to_index fi rest x)

let rec pick_fresh_var vars ?(suffix = "'") x =
  if is_var_bound vars x then pick_fresh_var vars ~suffix:suffix (x^suffix)
  else (x::vars), x

(* ---------------------------------------------------------------------- *)
(* Sanity checking of Debruijn *)

let sanity_check_from_var_list vlist = vlist
  (* List.length vlist *)
let sanity_check_from_ctx ctx = var_list_from_ctx ctx

let check_sane_vars fi vars x check =
  List.length vars = List.length check

(*    pr (index_to_var fi vars x)
  else
    pr ("[******************bad index: "
        ^ (string_of_int x)
            ^ "/" ^ (string_of_int (List.length check))
            ^ " in {"
            ^ (List.fold_left (fun s x -> s ^ " " ^ x) "" vars)
            ^ " }]") *)

let check_sane_ctx fi ctx x check =
  check_sane_vars fi (var_list_from_ctx ctx) x check

let shift_check check shift_amt =
  let rec pop_n n li =
    if n = 0 then li else
      pop_n (n-1) (List.tl li)
  in
  let rec push_n n li =
    if n = 0 then li else
      push_n (n-1) ("?" :: li)
  in

  if shift_amt >= 0
  then push_n shift_amt check
  else pop_n (-shift_amt) check
    (* check + shift_amt *)

let add_checked_var check x =
  x::check (* check + 1 *)

let sanity_check_string fi vars x check =
  let correct_name = index_to_var fi check x in
  sprintf "%i/%i (%s) parsed in {%s} used in {%s}"
    x
    (List.length check)
    correct_name
    (String.concat " " check)
    (String.concat " " vars)

let name_to_term fi ctx x =
  TmVar(fi, name_to_index fi ctx x, sanity_check_from_ctx ctx)

let var_to_term fi vars x =
  TmVar(fi, var_to_index fi vars x, sanity_check_from_var_list vars)

(* ---------------------------------------------------------------------- *)
(* Shifting *)

let tmmap onvar onplaceholder c t =
  let rec walk c t = match t with
    | TmPrimitive _ -> t

    | TmVar(fi, x, n) -> onvar fi c x n
    | TmFun(fi, x, tyT1, t2) -> TmFun(fi, x, walk c tyT1, walk (c + 1) t2)
    | TmLet(fi, x, t1, t2) -> TmLet(fi, x, walk c t1, walk (c + 1) t2)
    | TmArrow(fi, x, tyS, tyT) -> TmArrow(fi, x, walk c tyS, walk (c + 1) tyT)
    | TmApp(fi, t1, t2) ->
        TmApp(fi, walk c t1, walk c t2)
    | TmPlaceHolder(fi,p,s) -> onplaceholder fi c p s
  in walk c t

let rec term_shift_above (d:int) (c:int) (t:term):term =
  tmmap
    (fun fi c x n ->
       if x >= c
       then TmVar(fi, x + d, shift_check n d)
       else TmVar(fi, x, shift_check n d))
    (fun fi c p sub ->
       TmPlaceHolder (fi, p, List.map (term_shift_above d c) sub))
    c t

let term_shift d t = term_shift_above d 0 t

let binding_shift d bind =
  match bind with
    | VarBind(tyT, None) -> VarBind(term_shift d tyT, None)
    | VarBind(tyT, Some(tm)) ->
        VarBind(term_shift d tyT, Some(term_shift d tm))

(* ---------------------------------------------------------------------- *)
(* Substitution *)

let rec subst_tm_in_tm j s t =
  tmmap
    (fun fi j x n -> if x = j then (term_shift j s) else TmVar(fi, x, n))
    (fun fi j p sub ->
       TmPlaceHolder (fi, p, List.map (subst_tm_in_tm j s) sub))
    j t

let subst_top_tm_in_tm s t =
  term_shift (-1) (subst_tm_in_tm 0 (term_shift 1 s) t)

(* ---------------------------------------------------------------------- *)
(* Delayed Substitution *)

let apply_delayed_subst sub t =
  Support.foldi_left
    (fun i tm next_tm -> subst_tm_in_tm i next_tm tm)
    t
    sub

exception HasDelayed
let has_delayed_subst t =
  try
    (* Passing 0 is OK because we never use it *)
    ignore (tmmap
              (fun fi c x n -> TmVar(fi, x, n))
              (fun fi c p s ->
                 Support.foldi_left
                   (fun i () tm ->
                      match tm with
                      | TmVar (_,x,_) when x = i -> ()
                      | _ -> raise HasDelayed)
                   () s;
                 TmPlaceHolder (fi,p,s))
              0
              t);
    false
  with
  | HasDelayed -> true

let apply_replacement replacement tm =
  (* The replacements must be applied in order, so a single tmmap will
   * not do *)
  let rec apply_one (ctx,p,new_tm) tm =
    tmmap
      (fun fi j x n -> TmVar(fi,x,n))
      (fun fi j p' sub ->
         if p = p' then
           apply_delayed_subst sub new_tm
         else
           TmPlaceHolder (fi, p', List.map (apply_one (ctx,p,new_tm)) sub))
      0
      tm
  in
  List.fold_left (fun t r -> apply_one r t) tm replacement

(* ---------------------------------------------------------------------- *)
(* Extracting file info *)

let tm_info t = match t with
  | TmVar(fi, _, _)
  | TmFun(fi, _, _, _)
  | TmLet(fi, _, _, _)
  | TmArrow(fi, _, _, _)
  | TmApp(fi, _, _)
  | TmPrimitive(fi, _, _)
  | TmPlaceHolder(fi, _, _) -> fi

let replace_info t newfi =
  match t with
  | TmVar(_, x, c) -> TmVar(newfi, x, c)
  | TmFun(_, x, ty, body) -> TmFun(newfi, x, ty, body)
  | TmLet(_, x, tm, body) -> TmLet(newfi, x, tm, body)
  | TmArrow(_, x, arg, res) -> TmArrow(newfi, x, arg, res)
  | TmApp(_, t1, t2) -> TmApp(newfi, t1, t2)
  | TmPrimitive(_, name, labels) -> TmPrimitive(newfi, name, labels)
  | TmPlaceHolder(_, name, subst) -> TmPlaceHolder(newfi, name, subst)


(* Context management (continued) *)

let rec get_binding fi ctx i =
  try
    let (_, bind) = List.nth ctx i in
    binding_shift (i+1) bind
  with Failure _ ->
    (*print_ctx ctx;*)
    let msg =
      Printf.sprintf "Variable lookup failure: offset: %d, ctxx size: %d" in
    error fi (msg i (List.length ctx))

let rec index_to_name fi ctx i =
  try
    let (name, bind) = List.nth ctx i in
    name
  with Failure _ ->
    (*print_ctx ctx;*)
    let msg =
      Printf.sprintf "Variable lookup failure: offset: %d, ctxx size: %d" in
    error fi (msg i (List.length ctx))

let get_ty_from_ctx fi ctx i =
  match get_binding fi ctx i with
     | VarBind(tyT, _) -> tyT

exception Variable_found

let contains_var t v =
  try
    let _ =
      tmmap
        (fun fi c x n ->
          if x == c+v then raise Variable_found;
          TmVar(fi, x, n))
        (fun fi c p sub -> TmPlaceHolder (fi, p, sub))
        0 t
    in false
  with Variable_found -> true

let make_as fi vars tm ty =
  TmApp(fi,
        (TmFun (fi, "x", ty,
                TmVar (fi, 0, sanity_check_from_var_list ("x"::vars)))),
        tm)

let make_lambda_sequence fi vars args ?ret:maybe_ret body =
  let arg_vars = List.rev_map fst args in
  let rec mk_sequence args =
    match args, maybe_ret with
    | [],           None        -> body
    | [],           Some ret_ty -> make_as fi (arg_vars @ vars) body ret_ty
    | (x,ty)::rest, _           -> TmFun (fi, x, ty, mk_sequence rest)
  in
  mk_sequence args

(* Precondition: the arg terms should have been parsed in proper order
   so this function doesn't have to shift them. *)
let make_arrow_sequence fi vars args ret =
  let rec mk_sequence args =
    match args with
    | []         -> ret
    | (var,ty) :: rest -> TmArrow(fi, var, ty, mk_sequence rest)
  in
  mk_sequence args

(* tm_info-insensitive equality *)

let rec term_eq t1 t2 = match (t1,t2) with
  | (TmVar(_, a, b), TmVar(_, a', b')) -> a = a'
  | (TmFun(_, i, a, b), TmFun(_, i', a', b')) ->
      (term_eq a a') && (term_eq b b')
  | (TmLet(_,v,a,b), TmLet(_,v',a',b')) ->
      v = v' && (term_eq a a') && (term_eq b b')
  | (TmArrow(_, i, a, b), TmArrow(_, i', a', b')) ->
      (term_eq a a') && (term_eq b b')
  | (TmApp(_, i, a), TmApp(_, i', a')) ->
      (term_eq i i') && (term_eq a a')
  | (TmPrimitive(_, s, pi), TmPrimitive(_, s', pi')) -> s = s' && pi = pi'
  | (TmPlaceHolder(_, p, s), TmPlaceHolder(_, p', s')) ->
      p = p' && (delayed_subst_eq s  s')
  | (_,_) -> false
and ctx_eq c1 c2 = match(c1, c2) with
  | ([], []) -> true
  | ((s1,vb1)::c1',(s2,vb2)::c2') ->
      (* s1 = s2 && *) (binding_eq vb1 vb2) && (ctx_eq c1' c2')
  | (_,_) -> false
and binding_eq (VarBind(ty1, b1)) (VarBind(ty2, b2)) =
      term_eq ty1 ty2 &&
      (match (b1,b2) with
      | (None, None) -> true
      | (Some(t1), Some(t2)) -> term_eq t1 t2
      | (_,_) -> false)
and delayed_subst_eq s1 s2 =
  match (s1, s2) with
  | ([],[]) -> true
  | (t1::s1',t2::s2') -> (term_eq t1 t2) && (delayed_subst_eq s1' s2')
  | (_,_) -> false

let rec get_app_args t =
  match t with
  | TmApp(_, t1, t2) -> get_app_args t1 @ [t2]
  | TmPrimitive(_, _, _) -> []
  | _ -> raise (Invalid_argument "get_app_args")

let rec valid_term ctx tm =
  match tm with
  | TmVar(fi, i, check) ->
      (i >= 0) && (i < List.length ctx) && (check_sane_ctx fi ctx i check)
  | TmFun(_, x, tyT, body) ->
      let ctx' = add_binding ctx x (VarBind(tyT, None)) in
        (valid_term ctx tyT) && (valid_term ctx' body)
  | TmArrow(_, x, tyS, tyT) ->
      let ctx' = add_binding ctx x (VarBind(tyT, None)) in
        (valid_term ctx tyS) && (valid_term ctx' tyT)
  | TmApp(_, t1, t2) -> (valid_term ctx t1) && (valid_term ctx t2)
  | TmPrimitive(_, name, labels) -> true
  | TmLet(_, x, t1, t2) ->
      let ctx' = add_binding ctx x (VarBind(t1, Some(t1))) in
        (valid_term ctx t1) && (valid_term ctx' t2)
  | TmPlaceHolder(_, ph, subst) -> true


(* ---------------------- *)

let tm_id_fn fi ctx ty =
  TmFun(fi, "x", ty,
        TmVar(fi, 0,
              sanity_check_from_var_list ("x"::(var_list_from_ctx ctx))))

let is_id_fn t =
  match t with
  | TmFun(_,_,_,TmVar(_,0,_)) -> true
  | _ -> false

(* apply f where possible to term t *)

let tm_walk f t =
  let rec walk t =
    match f t with
    | Some t' -> t'
    | None ->
        (match t with
        | TmPrimitive _
        | TmPlaceHolder _
        | TmVar _ ->
            t
        | TmFun(fi, x, tyT1, t2) -> TmFun(fi, x, walk tyT1, walk t2)
        | TmLet(fi, x, t1, t2) -> TmLet(fi, x, walk t1, walk t2)
        | TmArrow(fi, x, tyS, tyT) -> TmArrow(fi, x, walk tyS, walk tyT)
        | TmApp(fi, t1, t2) -> TmApp(fi, walk t1, walk t2))
  in walk t

let rec optimize t =
  let f t =
    match t with
    | TmApp(fi, TmApp(_, TmApp(_, TmPrimitive(_, "cast", []), tyS), tyT), t)
      when term_eq tyS tyT ->
        Some t
    | TmApp(fi, TmFun(_, _, _, TmVar(_, 0, _)), t) ->
        Some t
(* This is broken: causes things not to be sent to Simplify -Steve. *)
(*    | TmFun(fi, _, _, TmApp(_, s, TmVar(_, 0, _))) ->
        if contains_var s 0
        then (None)
        else (Some (term_shift (-1) s)) *)
    | _ -> None
  in
  let t' = tm_walk f t in
  if term_eq t t'
  then t
  else optimize t'

let is_arrow t =
  match t with
  | TmArrow(_, _, _, _) -> true
  | _ -> false
