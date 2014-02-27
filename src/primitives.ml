open Format
open Support
open Support.Error
open Support.Pervasive
open Syntax

(* ---------------------------------------------------------------------- *)
(* Primitives *)
let prim_list = ref []

let tm_prim ?(fi = dummyinfo) ?(labels = []) name =
  TmPrimitive(fi,name,labels)

let int_singleton = ref (function (n) -> tm_prim "Int")
let string_singleton = ref (function (s) -> tm_prim "Dynamic")

(* Pattern matching --- only depends on Syntax *)
let rec is_primitive tm =
  match tm with
  | TmPrimitive(_, _, _) -> true
  | TmApp(_, tm1, _) -> is_primitive tm1
  | _ -> false

let rec is_named_primitive tm name =
  match tm with
  | TmPrimitive(_, p, _) -> p = name
  | TmApp(_, tm1, _) -> is_named_primitive tm1 name
  | _ -> false

let rec prim_name tm =
  match tm with
  | TmPrimitive(_, p, _) -> p
  | TmApp(_, tm1, _) -> prim_name tm1
  | _ -> raise (Invalid_argument "prim_name")

let rec prim_labels tm =
  match tm with
  | TmPrimitive(_, _, labels) -> labels
  | TmApp(_, tm1, _) -> prim_labels tm1
  | _ -> raise (Invalid_argument "prim_labels")

let rec fully_split_arrow t =
  match t with
  | TmArrow(_, x, tyS, tyT) -> (x, tyS)::(fully_split_arrow tyT)
  | _ -> [("_", t)]
(* End pattern matching *)

let make_prim_app_sequence fi vars prim_name labels terms =
  let rec make_app left_tm rest =
    match rest with
    | arg1::arg2::args ->
        let new_tm = TmApp(fi, left_tm, arg1) in
        make_app new_tm (arg2::args)
    | [arg] -> TmApp(fi, left_tm, arg)
    | [] -> left_tm
  in
  let prim_tm = tm_prim ~fi ~labels prim_name in
    (make_app prim_tm terms)

(* prim_info:
 *   name
 *   type
 *   is_full_application_value? (true for constants that are already values)
 *   eval_fn
 *   cast_fn
 *)
let get_prim_info fi ctx name labels =
  try List.assoc name !prim_list with Not_found ->
  let id_fun = fun fi ctx labels args ->
    make_prim_app_sequence fi ctx name labels args in
  (* let null_cast = fun fi args labels ty v -> v in *)
  let error_cast = fun fi args labels ty v ->
    error fi "this cast should not happen" in
  let (full_value, eval_fn, ty, cast_fn) =
    match name with
    | n when is_numeric n -> (true, id_fun, !int_singleton n, error_cast)
    | s when s.[0] = '"' -> (true, id_fun, !string_singleton s, error_cast)
    | _ -> error fi ("unknown primitive: " ^ name)
  in
    {ty = ty; is_full_app_value = full_value;
     eval_fn = eval_fn; cast_fn = cast_fn}

let primitive_ty fi ctx name labels =
  term_shift (List.length ctx) (get_prim_info fi ctx name labels).ty

let primitive_arity fi ctx name labels =
  let ty = primitive_ty fi ctx name labels in
  match ty with
  | TmArrow(_, _, _, _) ->
    (List.length (fully_split_arrow ty)) - 1
  | _ -> 0

let rec is_full_prim_app fi ctx t =
  let rec is_full_app t c =
    match t with
    | TmApp(_, t1, v2) -> (is_full_app t1 (c + 1)) && (is_val fi ctx v2)
    | TmPrimitive(_, name, labels) ->
        c = (primitive_arity fi ctx name labels)
    | _ -> false
  in
    is_full_app t 0
and is_prim_val fi ctx t =
  match t with
  | TmPrimitive(_, _, _) -> true
  | TmApp(_, _, _) ->
      let rec walk_app tm c =
        match tm with
        | TmApp(_, left, right) ->
            (walk_app left (c + 1)) && (is_val fi ctx right)
        | TmPrimitive(_, name, labels) ->
            let arity = primitive_arity fi ctx name labels in
            let full_value =
              (get_prim_info fi ctx name labels).is_full_app_value
            in
              (c < arity) || ((c = arity) && full_value)
        | _ -> false
      in
        walk_app t 0
  | _ when is_full_prim_app fi ctx t ->
      let name = prim_name t in
      let labels = prim_labels t in
        (get_prim_info fi ctx name labels).is_full_app_value
  | _ -> false
and is_val fi ctx t =
  match t with
  | TmFun(_, _, _, _) -> true
  | TmArrow(_, _, _, _) -> true
  | TmPlaceHolder(_, _, _) -> true
  | TmVar(fi, n, _) ->
      (match get_binding fi ctx n with
       | VarBind(_, Some(t)) -> false
       | _ -> true)
  | _ when is_prim_val fi ctx t -> true
  | TmApp(_, TmApp(_, TmPrimitive(_, "fix", _), _), _)
    -> true
  | _ -> false

let make_cast fi ctx tyS tyT t =
  make_prim_app_sequence fi ctx "cast" [string_of_info fi] [tyS; tyT; t]

let make_refinement fi vars x ty tm =
  let tm' = TmFun(fi, x, ty, tm) in
    make_prim_app_sequence fi vars "Refine" [] [ty; tm']

(************************************)

let toplevel_parse_thunk = ref (fun _ v _ -> ([], v))
