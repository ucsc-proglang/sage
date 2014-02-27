open Format
open Support.Pervasive
open Support.Error
open Syntax
open Primitives
open Print
open Options

exception NoEvalRule of string * context * term

let eval_compiler =
  ref (fun ctx tm -> (tm, tm))

(* ---------------------------------------------------------------------- *)
(* Evaluation, by value *)

let rec eval_v1 ctx t =
    match t with
    | TmLet(fi, x, v1, t2) when is_val fi ctx v1 ->
        subst_top_tm_in_tm v1 t2
    | TmLet(fi, x, t1, t2) ->
        let t1' = eval_v1 ctx t1 in
        TmLet(fi, x, t1', t2)

     (* (fix U e) e' -> (e (fix U e)) e' *)
    | TmApp(fi,
            (TmApp(_,
                   TmApp(_,
                         TmPrimitive(_, "fix", []), _), fix_fn) as fix_exp),
            arg) ->
        TmApp(fi, TmApp(fi, fix_fn, fix_exp), arg)

     (* cast (fix U e) T -> cast (e (fix U e)) T *)
    | TmApp(fi1, TmApp(fi2,
                       (TmPrimitive(_, "cast", _) as cast),
                       (TmApp(_,
                              TmApp(_,
                                    TmPrimitive(_, "fix", []), _),
                              fix_fn) as fix_exp)),
            dest_ty) ->
        TmApp(fi1, TmApp(fi2, cast, TmApp(fi2, fix_fn, fix_exp)), dest_ty)

    | TmApp(fi, _, _) when is_full_prim_app fi ctx t ->
        let name = prim_name t in
        let labels = prim_labels t in
        let info = get_prim_info fi ctx name labels in
        let eval_fn = info.eval_fn in
        if info.is_full_app_value then
          raise (NoEvalRule("eval_v1",ctx,t))
        else
          let args = get_app_args t in
          eval_fn fi ctx labels args
    | TmApp(fi, TmFun(_, _, _, t1), v2) when is_val fi ctx v2 ->
        subst_top_tm_in_tm v2 t1
    | TmApp(fi, v1, t2) when (is_val fi ctx v1) && (not (is_val fi ctx t2)) ->
        let t2' = eval_v1 ctx t2 in
        TmApp(fi, v1, t2')
    | TmApp(fi, t1, t2) ->
        let t1' = eval_v1 ctx t1 in
        TmApp(fi, t1', t2)
    | TmVar(fi, n, _) ->
      (match get_binding fi ctx n with
      | VarBind(_, Some(t)) -> t
      | _ -> raise (NoEvalRule("eval_v1 -TmVar",ctx,t)))
    | _ -> raise (NoEvalRule("eval_v1",ctx,t))

let rec eval_v ctx t =
  if (Options.peval())
  then print_str_tm "evaluating: " ctx t;

  let t' =
    try
      let t' = eval_v1 ctx t in
      ignore (if Options.evalcomp() then !eval_compiler ctx t'
              else (t', t'));
      t'
    with NoEvalRule(_) ->
      if (Options.peval ())
      then print_str_tm "normal form: " ctx t;
      if not (is_val (tm_info t) ctx t)
      then print_str_tm "normal form is not a value: " ctx t;
      t
  in
  if not (term_eq t' t) then
    eval_v ctx t'
  else
    t'

(* ---------------------------------------------------------------------- *)
(* Evaluation, to whnf *)

let counter = ref 0

let inc() =
  counter := !counter + 1

let eval_reset_counter () =
  counter := 0

let eval_counter () =
  !counter

let rec eval_type_helper ctx t =
  let stuck () =
    debug "eval"
      (fun() ->
        if not (is_val dummyinfo ctx t)
        then print_str_tm "eval_type_helper: stuck: " ctx t);
    raise (NoEvalRule("eval_type_helper - TmApp2",ctx,t))
  in

  match t with
  | TmLet(fi, x, v1, t2) ->
      subst_top_tm_in_tm v1 t2

  (* (fix U e) e' -> (e (fix U e)) e' *)
  | TmApp(fi,
          (TmApp(_,
                 TmApp(_,
                       TmPrimitive(_, "fix", []),
                       _),
                 fix_fn) as fix_exp),
          arg) ->
            TmApp(fi, TmApp(fi, fix_fn, fix_exp), arg)

              (* cast (fix U e) T -> cast (e (fix U e)) T *)
  | TmApp(fi1, TmApp(fi2,
                     (TmPrimitive(_, "cast", _) as cast),
                     (TmApp(_,
                            TmApp(_,
                                  TmPrimitive(_, "fix", []),
                                  _),
                            fix_fn) as fix_exp)),
          dest_ty) ->
            TmApp(fi1, TmApp(fi2, cast, TmApp(fi2, fix_fn, fix_exp)), dest_ty)

  | TmApp(fi, _, _) when is_full_prim_app fi ctx t ->
      let name = prim_name t in
      let labels = prim_labels t in
      let info = get_prim_info fi ctx name labels in
      let eval_fn = info.eval_fn in
      if info.is_full_app_value
      then stuck()
      else
        let args = get_app_args t in
        eval_fn fi ctx labels args

  | TmApp(fi, t1, t2) ->
      (
        try
          let t1' = eval_type_helper ctx t1 in
          TmApp(fi, t1', t2)
        with NoEvalRule(_) -> try
          let t2' = eval_type_helper ctx t2 in
          TmApp(fi, t1, t2')
        with NoEvalRule(_) ->
          match t1 with
          | TmFun(_, _, _, t1b)
              (* We'd like to only beta-value here, but does not work for
                 quicksort.f
                 when is_val fi ctx t2
               *)
            ->
              subst_top_tm_in_tm t2 t1b (* beta *)
          | _ -> stuck()
      )

  | TmVar(fi, n, _) ->
      (match get_binding fi ctx n with
      | VarBind(_, Some(t)) -> t
      | VarBind(_,None) -> stuck())
  | _ -> stuck ()

let eval_type1 ctx tm =
  match tm with

  (* (fix U e) -> (e (fix U e))  *)
  | TmApp(fi,
          TmApp(_,
                TmPrimitive(_, "fix", []),
                _),
          fix_fn) ->
            TmApp(fi, fix_fn, tm)
  | _ -> eval_type_helper ctx tm

let eval_type_many n ctx t =
  let rec helper n t =
    if n = 0
    then t
    else
      try
        let tt = helper (n-1) (eval_type1 ctx t) in
        inc();
        tt
      with NoEvalRule(_) -> t
  in
  helper (n-1) (eval_type1 ctx t)

let eval_type ctx t =
  debug_wrapper "eval_type"
    (fun () -> print_str_tm "eval_type " ctx t)
    (fun r -> print_str_tm "eval_type " ctx r)
    (fun () ->
      try eval_type_many (Options.maxeval()) ctx t
      with NoEvalRule(_) -> t)

let rec simplify_type ctx t =
  debug_wrapper "simplify_type"
    (fun () -> print_str_tm "simplify_type " ctx t)
    (fun r -> print_str_tm "simplify_type " ctx r)
    (fun () ->
      match t with
      | TmApp(_,TmApp(_,TmPrimitive(_,"fix",[]),_),_) -> t
      | _ ->
          (match eval_type ctx t with
          | TmArrow(fi,x,tyS,tyT) ->
              let ctx' =  (x,(VarBind(tyS, None)))::ctx in
              TmArrow(fi,x,simplify_type ctx tyS, simplify_type ctx' tyT)
          | t -> t))
