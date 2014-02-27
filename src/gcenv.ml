open Set
open List
open Print
open Support.Pervasive
open Support.Error
open Format
open Syntax
open Options

type rev_map = int list

module IntMod =
  struct
    type t = int
    let compare x y = if x < y then -1 else if x > y then 1 else 0
  end

module IntSet = Set.Make(IntMod)

let flatten l =
  fold_left (fun set res -> IntSet.union set res) IntSet.empty l

let shift set n =
  IntSet.fold (fun elem res -> IntSet.add (elem + n) res) set IntSet.empty

let print_int_set l =
  pr "["; IntSet.iter (function (x) -> (Format.print_int x); pr " ") l; pr "]"

let print_var_set ctx l =
  let vars = var_list_from_ctx ctx in
  let name x = index_to_var dummyinfo vars x in
  pr "["; IntSet.iter (function (x) -> (pr (name x)); pr " ") l; pr "]"

let print_int_list l =
  pr "["; iter (function (x) -> (Format.print_int x); pr " ") l; pr "]"

let print_bool_list l =
  pr "["; iter (function (x) -> if x then pr "T " else pr "F ") l; pr "]"

let overlaps set1 set2 =
  not (IntSet.is_empty(IntSet.inter set1 set2))

(* MARK free vars of term or context *)

let free_vars_in_term t =
  let empty = IntSet.empty  in
  let rec helper t c =
    let ok x = (x - c >= 0) in
    match t with
    | TmPrimitive _ -> empty
    | TmVar(fi, x, n) ->
        if (ok x) then (IntSet.add (x-c) empty) else empty
    | TmFun(fi, x, tyT1, t2) ->
        IntSet.union (helper tyT1 c) (helper t2 (c+1))
    | TmLet(fi, x, t1, t2) ->
        IntSet.union (helper t1 c) (helper t2 (c+1))
    | TmArrow(fi, x, tyS, tyT) ->
        IntSet.union (helper tyS c) (helper tyT (c+1))
    | TmApp(fi, t1, t2) ->
        IntSet.union (helper t1 c) (helper t2 c)
    | TmPlaceHolder(fi,p,s) -> empty
  in
  helper t 0


let is_refinement t =
  match t with
  | TmApp(_, TmApp(_, TmPrimitive(_, "Refine", []), _), _) -> true
  | _ -> false

let rec free_in_ctx ctx n currently_used =
  let first_num = n+1 in
  match ctx with
  | [] -> currently_used
  | c::cs ->
      let (free_in_binding,ty) =
        match (c) with
        | (str, VarBind(ty, None)) ->
            (shift (free_vars_in_term ty) first_num, ty)
        | (str, VarBind(ty, Some(tm))) ->
            (flatten ((shift (free_vars_in_term ty) first_num)::
                     (shift (free_vars_in_term tm) first_num)::[]), ty)
      in
      if ((IntSet.mem n currently_used) || (is_refinement ty)) then
        free_in_ctx cs (first_num)
          (IntSet.add n (IntSet.union currently_used free_in_binding))
      else
        free_in_ctx cs (first_num) currently_used

let mark ctx terms =
  let orig_used = flatten (map free_vars_in_term terms) in
  debug "gcenv" (function () ->
    pr "[gcenv] roots: ";
    (print_int_set orig_used);
    force_newline());
  let rec mark_helper ctx used =
    let new_used = IntSet.union used (free_in_ctx ctx 0 used) in
    if (IntSet.equal used new_used) then
      used
    else mark_helper ctx new_used
  in
  mark_helper ctx orig_used


(* SWEEP *)

let build_map n used =
  let rec helper a total =
    if (a = n) then [],[],total
    else
      if (IntSet.mem a used) then
        let (rest,inverted,t) = helper (a + 1) (total + 1) in
        (total::rest),(a::inverted),t
      else
        let (rest,inverted,t) = helper (a + 1) (total) in
        ((-1)::rest),inverted,t
  in
  helper 0 0

let shift_vars t map num_free_vars =
  let rec helper t c =
  match t with
    | TmPrimitive _ -> t
    | TmVar(fi, x, n) ->
        if (x - c >= 0) then
          (TmVar(fi, (List.nth map (x - c)) + c,
                 shift_check
                   (sanity_check_from_var_list [])
                   (num_free_vars + c) ) )
        else
          TmVar(fi, x, shift_check
                         (sanity_check_from_var_list [])
                         (num_free_vars + c) )
    | TmFun(fi, x, tyT1, t2) ->
        TmFun(fi, x, helper tyT1 c, helper t2 (c + 1))
    | TmLet(fi, x, t1, t2) ->
        TmFun(fi, x, helper t1 c, helper t2 (c + 1))
    | TmArrow(fi, x, tyS, tyT) ->
        TmArrow(fi, x, helper tyS c, helper tyT (c + 1))
    | TmApp(fi, t1, t2) ->
        TmApp(fi, helper t1 c, helper t2 c)
    | TmPlaceHolder(fi,p,s) -> t
  in
  helper t 0

let shift_vars_opt t map total =
  match t with
  | None -> None
  | Some(t) -> Some(shift_vars t map total)

let rec shift_down list v =
  match list with
  | [] -> []
  | -1::ns -> -1::(shift_down ns v)
  | n::ns -> (n + v)::(shift_down ns v)

let apply_map_to_binding c map  num_free_vars =
  match c with
  | VarBind(ty, None) -> VarBind ((shift_vars ty map num_free_vars), None)
  | VarBind(ty, Some(tm)) ->
      VarBind ((shift_vars ty map num_free_vars),
               Some((shift_vars tm map num_free_vars)))

let rec shift_ctx (ctx:context) (shift_map: int list)
      (total:int) (keep: int list) =
  match (ctx, shift_map, keep) with
    ([], [], []) -> []
  | (c::cs, v::vs, k::ks) ->
      if (k < 0) then
        (shift_ctx cs vs total ks)
      else
        let shift_map = (shift_down vs (-1)) in
        let (str, bind) = c in
        (str, apply_map_to_binding bind shift_map (total - 1))::
        (shift_ctx cs shift_map (total - 1) ks)
  | _ -> [] (* should never happen... *)


(* GC *)

let gc_env_list ctx terms =
  if (Options.gcenv() & (List.length ctx) > 0) then (
    (*debug "gcenv" (fun () -> pr "[gcenv] orig ";
      print_subtype_test ctx s t ; force_newline());*)
    let used = mark ctx terms in
    let (map,inverted,total) = build_map (List.length ctx) used in
    let ctx' = (shift_ctx ctx map total map) in
    let terms' = List.map (fun t -> shift_vars t map total) terms in
    (*debug "gcenv" (fun () -> pr "[gcenv] final ";
      print_subtype_test ctx' s' t'; force_newline());*)
    (ctx', terms', inverted)
  )
  else
    (ctx, terms, [])

let gc_env_2 ctx s t =
  match gc_env_list ctx [s; t] with
  | ctx, [s; t], inv -> ctx, s, t, inv
  | _ -> failwith "Fatal error in gc_env_2: number of terms changed."

let gc_env_3 ctx s t u =
  match gc_env_list ctx [s; t; u] with
  | ctx, [s; t; u], inv -> ctx, s, t, u, inv
  | _ -> failwith "Fatal error in gc_env_3: number of terms changed."

let ungc_env inverted_map orig_ctx_size ctx u =
  if (Options.gcenv() & orig_ctx_size > 0) then
    shift_vars u inverted_map orig_ctx_size
  else u
