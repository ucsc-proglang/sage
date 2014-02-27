(* Module Main: The main program.  Deals with processing the command
   line, reading files, building and connecting lexers and parsers, etc.

   For most experiments with the implementation, it should not be
   necessary to change this file.
*)

open Format
open Support.Pervasive
open Support.Error
open Syntax
open Subtype
open Basicalg
open Compile
open Eval
open Primitives
open Print
open Prim_defs

let open_file in_file =
  let rec trynext l = match l with
      | [] -> err ("Could not find " ^ in_file)
      | (d::rest) ->
          let name = if d = "" then in_file else (d ^ "/" ^ in_file) in
          try open_in name
            with Sys_error m -> trynext rest
  in trynext (Options.search_path())

let parse_file in_file =
  let pi = open_file in_file
  in let lexbuf = Lexer.create in_file pi
  in let result =
    try Parser.toplevel Lexer.main lexbuf with Parsing.Parse_error ->
    error (Lexer.info lexbuf) "Parse error"
in
  Parsing.clear_parser(); close_in pi; result

let already_imported = ref ([] : string list)

let process_term ctx fi t =
  if Options.steps() then
    print_str_tm "Source: " ctx t;
  
  let (t', tyT) = compile_tm ctx t in
  if Options.steps() then print_str_tm "Compiled: " ctx t';

  (*print_str_tm "Type: " ctx tyT;*)
  
  (*if Options.steps () then  *)
  let tyT' = (* simplify_type ctx *) tyT in
  (*if term_eq tyT tyT'
  then ()
  else*)
  print_str_tm "Type: " ctx tyT';
  

  let t'= optimize t' in
  if Options.steps() then
    print_str_tm "Optimized: " ctx t';

  let t'' =
    if (num_warnings() = 0) && (not (Options.noeval())) then
      (
        pr "Evaluation: ";
        let t'' = eval_v ctx t' in
        (* if Options.steps() then *)
        print_str_tm "" ctx t'';
        t''
      )
    else
      (
        if Options.steps() && false then (
          if (not (Options.noeval())) then
            (pr "Warnings Found. Skipping Evaluation\n")
          else
            (pr "Skipping Evaluation (-noeval)\n")
        );
        t'
      );
  in
  t'', tyT

let rec process_command ctx cmd =
  match cmd with
  | Define (fi, x, t) ->
      if Options.steps() || true then (
        pr "Binding for: ";
        pr x;
        print_newline ()
      );
      let tm, ty = process_term ctx fi t in
      add_binding ctx x (VarBind (ty, Some tm));
      
  | Eval (fi, t) ->
      (* pr "Typechecking anonymous term\n"; *)
      ignore (process_term ctx fi t);
      ctx

  | Assume (subty_ctx, ty1, ty2, valid) ->
      let subty_ctx = subty_ctx @ ctx in
      (*let db = new Db.database db_name in*)
      (* TODO: ensure subtype_ctx is well formed *)
      let fi = tm_info ty1 in
      let ty1,k1 = process_term subty_ctx fi ty1 in
      let ty2,k2 = process_term subty_ctx fi ty2 in
      (* TODO: Singleton types break this check. We should fix it. *)
      (*
        if (not (term_eq k1 (tm_prim "*")))
        || (not (term_eq k2 (tm_prim "*"))) then
        warning fi "Term in 'assume' clause not of type '*'";
      *)


      (* let id = tm_id_fn (tm_info ty1) subty_ctx ty2 in *)
      if valid then (
            if (Options.checkassumes()) then (
              (match (Subtype.is_subtype fi subty_ctx ty1 ty2) with
               | False _ -> warning fi "algorithm indicates assume is bad";
                       Subtype.valid_subtype (tm_info ty1) subty_ctx ty1 ty2
               | True ->
                       warning fi "algorithm already knows this is true"
           | Maybe ->
                   Subtype.valid_subtype (tm_info ty1) subty_ctx ty1 ty2)
            )
            else
              Subtype.valid_subtype (tm_info ty1) subty_ctx ty1 ty2
      )
      else (
        if (Options.checkassumes()) then (
          match (Subtype.is_subtype fi subty_ctx ty1 ty2) with
          | False _ ->
                  warning fi "algorithm already knows this is false"
          | True ->
                  pr "algorithm indicates this assumption is true "
          | Maybe -> ()
        );
        Subtype.invalid_subtype (tm_info ty1) subty_ctx ty1 ty2 None
      );
      ctx
  | Check _ | Prove _ -> failwith "Check and prove not implemented."

let process_file f ctx =
  already_imported := f :: !already_imported;
  pr ("Parsing: " ^ f); print_newline ();
  let (cmds, _) = parse_file f (var_list_from_ctx ctx) prim_name_list in
  pr "Parsed:"; print_newline ();
  let g ctx c =
    open_hovbox 0;
    let results = process_command ctx c in
    close_box ();
    print_flush();
    results
  in
  List.fold_left g ctx cmds


let toplevel ctx =
  let ends_with_semis = Str.regexp ".*;;" in
  
  let rec read_until_twosemis initial fragment =
    if not initial then pr "  "; print_flush ();
    let input = try fragment ^ (read_line ()) with End_of_file -> exit 0 in
    let len = String.length input in
    if Str.string_match ends_with_semis input 0 then
      input
    else
      read_until_twosemis false input
  in
  
  let parse_and_run_cmds (ctx:context) str =
    let lexbuf = Lexing.from_string str in
    try
      let parsed_fun = Parser.toplevel Lexer.main lexbuf in
      List.fold_left 
        process_command  
        ctx
        (fst 
           (parsed_fun (var_list_from_ctx ctx) prim_name_list)) 
    with Parsing.Parse_error ->
      error (Lexer.info lexbuf) "Parse error"
  in

  let rec toploop ctx =
    let newctx = try
      pr "# "; print_flush ();
      let commands = (read_until_twosemis true "") in
      parse_and_run_cmds ctx commands
    with
    | ex -> pr (Printexc.to_string ex); pr "\n"; ctx
    in
    toploop newctx
  in
  pr "        Welcome to Sage\n\n";
  toploop ctx


let algorithm_list () =
  if (Options.stupid()) then [] else [ Basicalg.basic_subtype ]

let main () =
  let in_files = Options.parse_args() in
  let absolute_filenames = 
    List.map (fun s -> Filename.concat (Sys.getcwd()) s) in_files
  in
  let () = set_max_boxes 1000 in
  let () = set_margin (Options.width()) in
  let _ = init_subtype (algorithm_list()) (Options.db()) absolute_filenames in
  try
    let ctx =
      if Options.loadprelude() then
        process_file (Options.prelude()) empty_ctx
      else
        empty_ctx
    in

    let _ = 
      match in_files with
      | [] -> toplevel ctx
      | _  -> List.fold_left
          (fun ctx filename -> process_file filename ctx)
            ctx
            in_files
    in
    fini_subtype();
    ()
  with x ->
    fini_subtype();
    raise x

let () =
  set_max_boxes 1000;
  set_margin 67;
  let res =
    try
      main();
      num_warnings()
    with Exit x -> x
  in
  pr (Printf.sprintf "Total Warnings:   %d" (num_warnings()));
  pr "\n\n";
  print_flush ();
  Options.call_at_exits ();
  exit res
