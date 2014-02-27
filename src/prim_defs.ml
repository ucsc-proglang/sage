open Format
open Support
open Support.Error
open Support.Pervasive
open Syntax
open Print
open Primitives
open Options

open Eval

let prim_name_list = [
  "*"; "Top"; "Unit"; "unit"; "Bool"; "true"; "false"; "not"; "Int"; "add";
  "sub"; "geq"; "gt"; "lt"; "leq"; "IF"; "cast"; "Refine"; "Dynamic"; "fix";
  "mul"; "and"; "or"; "implies"; "inteq"; "iff"; "eq";
  (* Please forgive me *)
  "print"; "newline"
]

let parse_term_string vars s =
  let lexbuf = Lexing.from_string s in
  let result =
    try
      Parser.term Lexer.main lexbuf
    with Parsing.Parse_error ->
      error (Lexer.info lexbuf) ("Internal string parsing error: " ^ s)
  in
  Parsing.clear_parser();
  result vars prim_name_list;;


let parse_toplevel_string s =
  let lexbuf = Lexing.from_string s in
  let result =
    try
      Parser.toplevel Lexer.main lexbuf
    with Parsing.Parse_error ->
      error (Lexer.info lexbuf) ("parse_toplevel_string - bad string: " ^ s)
  in
  result;;

Primitives.toplevel_parse_thunk := parse_toplevel_string;;

let stuck_prim name ctx args =
  raise (NoEvalRule ("stuck primitive",
                     ctx,
                     make_prim_app_sequence
                       dummyinfo (var_list_from_ctx ctx) name [] args));;

let domain fi ctx v =
  match v with
  | TmFun(_,_,t,_) -> t
  | TmApp(_,TmApp(_,TmPrimitive(_,"fix",[]),
                  TmArrow(_,_,t,_)),
          _) -> t
  | _ ->
      print_str_tm
        "Unknown function value in cast, maybe eta-expand: " ctx v;
      error fi "Unknown function value in cast";;

let error_eval = fun fi ctx labels args ->
  error fi "this should not be evaluated"

let error_cast = fun name fi ctx labels args ty v ->
  let vars = var_list_from_ctx ctx in
  pr "This cast should not happen; from:";
  print_newline ();
  print_tm vars ty;
  print_newline ();
  pr "to:";
  print_newline ();
  pr name; pr " "; List.iter (print_tm vars) args;
  print_newline ();
  pr "on:";
  print_newline ();
  print_tm vars v;
  print_newline ();
  raise (NoRuleApplies("Bad cast"))

let null_cast = fun fi ctx labels args ty v -> v

let cast_failure fi ctx labels t1 t2 v msg =
  let vars = var_list_from_ctx ctx in
  pr "Failed cast:";
  print_newline();
  pr "  From: ";
  print_tm vars t1;
  print_newline();
  pr "  To: ";
  print_tm vars t2;
  print_newline();
  pr "  On: ";
  print_tm vars v;
  print_newline();
  pr "  With label: ";
  if labels = [] then pr "none" else pr (List.hd labels);
  print_newline();
  Subtype.invalid_subtype fi ctx t1 t2 (Some(v));
  error fi ("cast failure " ^ msg)

let boolop (n:string) (f:(bool->bool->bool)) =
    (n, {
      ty = parse_term_string []
             ("x:Bool->y:Bool->(z:Bool.(z<=>("^n^" x y)))");
      is_full_app_value = false;
      eval_fn = (fun fi ctx labels args ->
        match args with
        | [left; right] when (is_primitive left) && (is_primitive right) ->
            let left_val = bool_of_string (prim_name left) in
            let right_val = bool_of_string (prim_name right) in
            tm_prim ~fi (string_of_bool (f left_val right_val))
        | _ -> stuck_prim n ctx args);
      cast_fn = error_cast n
    });;
let intop (n:string) (f:int->int->int) =
    (n, {
      ty = parse_term_string [] ("x:Int->y:Int->(z:Int.(z=("^n^" x y)))");
      is_full_app_value = false;
      eval_fn = (fun fi ctx labels args ->
        match args with
        | [left; right] when (is_primitive left) && (is_primitive right) ->
            let left_val = int_of_string (prim_name left) in
            let right_val = int_of_string (prim_name right) in
            tm_prim ~fi (string_of_int (f left_val right_val))
        | _ -> stuck_prim n ctx args);
      cast_fn = error_cast n
    });;
let intcmpop (n:string) (f:int->int->bool) =
    (n, {
      ty = parse_term_string [] ("x:Int->y:Int->(b:Bool. b <=> ("^n^" x y))");
      is_full_app_value = false;
      eval_fn = (fun fi ctx labels args ->
        match args with
        | [left; right] when (is_primitive left) && (is_primitive right) ->
            let left_val = int_of_string (prim_name left) in
            let right_val = int_of_string (prim_name right) in
            if f left_val right_val then
              tm_prim ~fi "true"
            else
              tm_prim ~fi "false"
        | _ -> stuck_prim n ctx args);
      cast_fn = error_cast n
   });;

let prim_definitions =
  [
    ("*", {
      ty = tm_prim "*";
      is_full_app_value = true;
      eval_fn = error_eval;
      cast_fn = error_cast "*"
    });
    ("Top", {
      ty = tm_prim "*";
      is_full_app_value = true;
      eval_fn = error_eval;
      cast_fn = null_cast
    });
    ("Unit", {
      ty = tm_prim "*";
      is_full_app_value = true;
      eval_fn = error_eval;
      cast_fn = fun fi ctx labels args ty v ->
        if is_named_primitive v "unit" then v
        else
          let to_ty = make_prim_app_sequence fi
                         (var_list_from_ctx ctx) "Unit" [] args in
          cast_failure fi ctx labels ty to_ty v "(cast value to Unit)"
    });
    ("unit", {
      ty = tm_prim "Unit";
      is_full_app_value = true;
      eval_fn = error_eval;
      cast_fn = error_cast "unit";
    });
    ("Bool", {
      ty = tm_prim "*";
      is_full_app_value = true;
      eval_fn = error_eval;
      cast_fn = fun fi ctx labels args ty v ->
        if is_named_primitive v "true" then v
        else if is_named_primitive v "false" then v
        else
          let to_ty = make_prim_app_sequence fi
                        (var_list_from_ctx ctx) "Bool" [] args in
            cast_failure fi ctx labels ty to_ty v "(cast value to Bool)"
    });
    ("true", {
      ty = parse_term_string [] "(b:Bool.b)";
      is_full_app_value = true;
      eval_fn = error_eval;
      cast_fn = error_cast "true"
    });
    ("false", {
      ty = parse_term_string [] "(b:Bool.not b)";
      is_full_app_value = true;
      eval_fn = error_eval;
      cast_fn = error_cast "false"
    });
    ("not", {
      ty = parse_term_string [] "(b:Bool->(c:Bool. c <=> (not b)))";
      is_full_app_value = false;
      eval_fn = (
        fun fi ctx labels args ->
          match args with
          | [arg] ->
              if is_named_primitive arg "true" then tm_prim ~fi "false"
              else if is_named_primitive arg "false" then tm_prim ~fi "true"
              else stuck_prim "not" ctx args
          | _ -> stuck_prim "not" ctx args);
      cast_fn = error_cast "not"
     });

    ("Int", {
      ty = tm_prim "*";
      is_full_app_value = true;
      eval_fn = error_eval;
      cast_fn = fun fi ctx labels args ty v ->
        if (is_primitive v) && (is_numeric (prim_name v)) then v
        else
          let to_ty = make_prim_app_sequence fi
                        (var_list_from_ctx ctx) "Int" [] args in
            cast_failure fi ctx labels ty to_ty v "(cast value to Int)"
    });

    (boolop "and" (fun x y -> x && y));
    (boolop "or" (fun x y -> x || y));
    (boolop "implies" (fun x y -> (not x) || y));
    (boolop "iff" (fun x y -> x=y));

    (intop "add" (fun x y -> x+y));
    (intop "sub" (fun x y -> x-y));
    (intop "mul" (fun x y -> x*y));

    (intcmpop "inteq" (fun x y -> x=y));
    (intcmpop "leq" (fun x y -> x<=y));
    (intcmpop "geq" (fun x y -> x>=y));
    (intcmpop "lt" (fun x y -> x<y));
    (intcmpop "gt" (fun x y -> x>y));

    ("eq", {
      ty = parse_term_string [] "Dynamic->Dynamic->Bool";
      is_full_app_value = false;
      eval_fn = (fun fi ctx labels args ->
        match args with
        | [left; right] ->
            if term_eq left right then
              tm_prim ~fi "true"
            else
              tm_prim ~fi "false"
        | _ -> stuck_prim "eq" ctx args);
      cast_fn = error_cast "eq";
    });
    ("IF", {
      ty = parse_term_string []
             ("X:*->p:Bool->((if_dummy_then:Unit.p)->X)->" ^
             "((if_dummy_else:Unit.not p)->X)->X");
      is_full_app_value = false;
      eval_fn = (fun fi ctx labels args ->
        match args with
        | [ty; grd; thn; els] ->
            if is_named_primitive grd "true" then
              TmApp(fi, thn, tm_prim "unit")
            else
              TmApp(fi, els, tm_prim "unit")
        | _ -> stuck_prim "IF" ctx args);
      cast_fn = error_cast "IF"
    });
    ("cast", {
      ty = parse_term_string [] "X:*->Y:*->X->Y";
      is_full_app_value = false;
      eval_fn =
         (fun fi ctx labels args ->
            match args with
            | [tyS; tyT; v] ->
                    if (Options.pcasts()) then (
                      print_str_tm "Casting value: " ctx v;
                      print_str_tm "from type: " ctx tyS;
                      print_str_tm "to type: " ctx tyT;
                    );

                (match (tyS, tyT) with
                 | (_, TmArrow(_, x, tyS, tyT)) ->
                             let castarg = "castarg" in
                     let ctx' = add_binding ctx castarg
                                  (VarBind(tyS, None))
                     in
                     let domain_ty = domain fi ctx v in
                     let inner_cast =
                       make_cast fi ctx'
                         (term_shift 1 tyS)
                         (term_shift 1 domain_ty)
                         (name_to_term fi ctx' castarg)
                     in
                     let app = TmApp(fi, term_shift 1 v,inner_cast) in
                     let outer_cast = make_cast fi ctx'
                                        (TmPrimitive(fi, "Dynamic", []))
                                        tyT app
                     in
                     TmFun(fi, castarg, tyS, outer_cast)
                 | (_, _) when term_eq tyS tyT -> v
            | (_, _) when is_full_prim_app fi ctx tyT ->
                let name = prim_name tyT in
                let labels = prim_labels tyT in
                let cast_fn =
                  (get_prim_info fi ctx name labels).cast_fn
                in
                let args = get_app_args tyT in
                cast_fn fi ctx labels args tyS v
            | (_, _) ->
                cast_failure fi ctx labels tyS tyT v "raw")
        | _ -> stuck_prim "cast" ctx args);
      cast_fn = error_cast "cast"
    });
    ("Refine", {
      ty = parse_term_string [] "X:*->(X->Bool)->*";
      is_full_app_value = true;
      eval_fn = error_eval;
      cast_fn = fun fi ctx labels args tyS v ->
        let refinement = make_prim_app_sequence fi
                           (var_list_from_ctx ctx) "Refine" [] args
        in
        match args with
        | [_; fn] ->
                make_prim_app_sequence fi
                  (var_list_from_ctx ctx) "RefineInProgress" labels
                  [tyS; refinement; v; TmApp(fi,fn,v)]
        | _ -> stuck_prim "Refine" ctx args
     });
    ("RefineInProgress", {
       ty = parse_term_string []
         "T:*->X:*->T->Bool->X";
       is_full_app_value = false;
       eval_fn = (
         fun fi ctx labels args ->
           match args with
           | [ty; ref; v; t] when is_named_primitive t "true" -> v
           | [ty; ref; v; t] when is_named_primitive t "false" ->
               cast_failure fi ctx labels ty ref v "(refine in progress)"
           | _ -> stuck_prim "RefineInProgress" ctx args);
       cast_fn = error_cast "RefineInProgress";
     });
    ("Dynamic", {
       ty = parse_term_string [] "*";
       is_full_app_value = true;
       eval_fn = error_eval;
       cast_fn = fun di ctx labels args ty v -> v
     });
    ("fix",
    let eval_fix =
      fun di ctx args ->
        match args with
        | [fix_ty; fn] ->
                TmApp(di, fn, TmApp(di, TmApp(di, tm_prim "fix", fix_ty), fn))
        | _ -> stuck_prim "fix" ctx args
    in
    {
      ty = parse_term_string [] "X:*->(X->X)->X";
      is_full_app_value = true;
      eval_fn = error_eval;
      cast_fn = fun fi ctx labels args src_ty casted_val ->
            make_cast fi ctx src_ty (eval_fix fi ctx args) casted_val
    });
   (* This is against the rules, but I *cannot* read the printed form of
      an encoded list.  This primitive is 100% ad-hoc and crummy. *)
   ("print",
    {
      ty = parse_term_string [] "Dynamic->Unit";
      is_full_app_value = false;
      eval_fn = (
        fun fi ctx labels args ->
          List.iter
            (fun tm ->
               if (is_primitive tm) && ((prim_name tm).[0] = '"') then
                 let name = prim_name tm in
                 pr (String.sub name 1 ((String.length name) - 2))
               else (
                 print_tm (var_list_from_ctx ctx) tm;
                 print_space ()))
            args;
          tm_prim "unit");
      cast_fn = error_cast "print";
    });
   ("newline",
    {
      ty = parse_term_string [] "Unit->Unit";
      is_full_app_value = false;
      eval_fn = (fun fi ctx labels args ->
                   force_newline (); flush stdout;
                   tm_prim "unit");
      cast_fn = error_cast "newline";
    });
  ]

let () = prim_list := prim_definitions

let () =
  int_singleton :=
    (function (n) ->
      if (int_of_string n) < 0 then
        parse_term_string []
          ("(Refine Int (fn (x:Int) => (inteq x (0"^n^"))))")
      else
        parse_term_string []
          ("(Refine Int (fn (x:Int) => (inteq x "^n^")))"))
