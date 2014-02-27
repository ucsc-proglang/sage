open Support.Error
open Support.Pervasive
open Primitives
open Prim_defs
open Syntax
open Support
open Simplify
open Print
open Options
open Format
open Eval

(*------- TODO
   set incomplete_translation in various places

   fresh variables may contain an apostrophe but can't by the simplify input
  -------*)


(*------- STRING CREATION FUNCTIONS -------*)
let unary_s command w1 = ("("^command^" "^w1^")")
let binary_s command w1 w2 = ("("^command^" "^w1^" "^w2^")")
let s_add w1 w2 = binary_s "+" w1 w2
let s_and w1 w2 = binary_s "AND" w1 w2
let s_eq w1 w2 = binary_s "EQ" w1 w2
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

let global_pred = ref("nonsense")
let incomplete_translation = ref false
let add_global_pred w = global_pred := !global_pred^" "^w


(*------ simplify renaming ------*)
let get_fresh_simplify_name =
  let var_counter = ref 0 in
  fun (v:variable) ->
    let nuv = Str.global_replace (Str.regexp "[^a-zA-Z0-9_]") "" v in
    var_counter := !var_counter + 1;
    let firstchar = Str.first_chars nuv 1 in
    if firstchar = "_"
    then "alphanum_"^(string_of_int !var_counter)
    else nuv^"_"^(string_of_int !var_counter)


let get_refine_args t =
  if is_named_primitive t "Refine"
  then match t with
  | TmApp(_,TmApp(_,TmPrimitive(_,"Refine",_),ty),TmFun(_,x,ty',body))
      -> (x, ty, body)
  | _ -> error (tm_info t) "not a primitive"
  else error (tm_info t) "not a primitive"

let rec get_simplify_name_exp ctx t =
  let rec f ctx t =
    match t with
    | TmVar(fi,i,_) -> index_to_name fi ctx i
(*    | TmApp(_,t1,t2) -> "APP_"^(f ctx t1)^"_"^(f ctx t2) *)
    | _ -> raise (NoRuleApplies "cannot translate")
  in
  try f ctx t
  with NoRuleApplies(_) -> get_fresh_simplify_name "digest_default_var";;
    

(*---digest functions ---*)
let rec digest_default t ctx =
  debug_wrapper "sformula" 
    (fun () -> print_str_tm "digest_default: " ctx t)
    (fun str -> pr ("digest_default result: "^str))
    (fun () -> 
      match t with
      | TmVar(fi,index,check) ->
          debug_pr "sformula" "Is a variable\n";
          if check_sane_ctx fi ctx index check then
            (* is this the correct var list? *)
	        index_to_name fi ctx index
          else
            error dummyinfo 
              ("[*********bad index: "
		       ^ (sanity_check_string fi (var_list_from_ctx ctx) index check)
		       ^ "]")
      |_ -> 
          (*debug_pr "sformula" "Going to default.\n";*)
          let ty = Compile.typeof ctx t in

          (match ty with
          | TmPrimitive(_, "*",_) -> ()
          | _ -> 
              incomplete_translation := true;
              debug "sformula" (fun () -> 
                print_str_tm "incomplete_translation term is: " ctx t;
                print_str_tm "type of incomplete_translation term is: " ctx ty;
                               );
          );

          let z = get_simplify_name_exp ctx t in
          let w_1 = digest_binding z (VarBind(ty,None)) ctx in
          add_global_pred w_1; 
          z
    ) 

and digest_int_term t ctx =
  debug_wrapper "sformula" 
    (fun () -> print_str_tm ("digest_int: ") ctx t)
    (fun str -> pr ("digest_int result: "^str)) 
    (fun () -> 
      if is_primitive t  (*&& is_full_prim_app dummyinfo ctx t*)
      then
	match prim_name t with
	|"add" -> 
	    (match get_app_args t with
	    | [left; right] -> 
		let w1 = digest_int_term left ctx in
		let w2 = digest_int_term right ctx in
		s_add w1 w2
	    | _ -> raise (Invalid_argument "digest_int_term add"))
	|"sub" -> 
	    (match get_app_args t with
	| [left; right] -> 
	    let w1 = digest_int_term left ctx in
	    let w2 = digest_int_term right ctx in
	    s_sub w1 w2
	| _ -> raise (Invalid_argument "digest_int_term sub"))
    |"mul" -> (match get_app_args t with
      | [left; right] -> 
	    let w1 = digest_int_term left ctx in
	    let w2 = digest_int_term right ctx in
	    s_mul w1 w2
      | _ -> raise (Invalid_argument "digest_int_term mul"))
    |"IF" -> (match get_app_args t with
      | [ty;pred;left;right] -> 
	 let w1 = digest_int_term pred ctx in
	  let w2 = digest_int_term left ctx in
	  let w3 = digest_int_term right ctx in
	  let z = get_fresh_simplify_name "if_int" in
	  let assertion = 
	    s_and (s_imp w1 (s_eq z w2)) (s_imp (s_not w1) (s_eq z w3)) in
	  add_global_pred assertion;
	  z
      | _ -> raise (Invalid_argument "digest_int_term IF"))
    | name ->
	if is_numeric name 
	then name
	else digest_default t ctx
  else 
    digest_default t ctx
    ) 

and digest_bool_term t ctx=
  debug_wrapper "sformula" 
    (fun () -> print_str_tm ("digest_bool: ") ctx t)
    (fun str -> pr ("digest_bool result: "^str)) 
    (fun () -> 
  if is_primitive t (*&& is_full_prim_app dummyinfo ctx t*)
  then
    begin
    (*debug_pr "sformula" ("digest bool: is primitive "^(prim_name t)^"\n");*)
    (match prim_name t with
    |"true" -> s_true()
    |"false" -> s_false()
    |"iff" -> 
	(match get_app_args t with
	| [left;right] ->
	    let w1 = digest_bool_term left ctx in
	    let w2 = digest_bool_term right ctx in
	    s_iff w1 w2
	|_ -> raise (Invalid_argument "digest_bool iff"))
    |"not" -> 
	(match get_app_args t with
	| [arg] ->
	    let w = digest_bool_term arg ctx in
	    s_not w
	|_ -> raise (Invalid_argument "digest_bool not"))
    |"and" -> 
	(match get_app_args t with
	| [left;right] ->
	    let w1 = digest_bool_term left ctx in
	    let w2 = digest_bool_term right ctx in
	    s_and w1 w2
	|_ -> raise (Invalid_argument "digest_bool and"))
    |"implies" -> 
	(match get_app_args t with
	| [left;right] ->
	    let w1 = digest_bool_term left ctx in
	    let w2 = digest_bool_term right ctx in
	    s_imp w1 w2
	|_ -> raise (Invalid_argument "digest_bool and"))
    |"eq" -> 
	(match get_app_args t with
	| [left;right] ->
            let t = Compile.typeof ctx left in
            let t = Compile.strip_refine_and_eval dummyinfo ctx t in
            (match t with
            | TmPrimitive(_, "Bool", []) ->
                (* pr "eq-Bool\n "; *)
	        let w1 = digest_bool_term left ctx in
	        let w2 = digest_bool_term right ctx in
                s_iff w1 w2
            | _ ->
                (* pr "eq-Int\n "; *)
	        let w1 = digest_int_term left ctx in
	        let w2 = digest_int_term right ctx in
                s_eq w1 w2)
	|_ -> raise (Invalid_argument "digest_bool eq"))
    |"inteq" -> 
	(match get_app_args t with
	| [left;right] ->
	    let w1 = digest_int_term left ctx in
	    let w2 = digest_int_term right ctx in
	    s_eq w1 w2
	|_ -> raise (Invalid_argument "digest_bool inteq"))
    |"geq" -> 
	(match get_app_args t with
	| [left;right] ->
	    let w1 = digest_int_term left ctx in
	    let w2 = digest_int_term right ctx in
	    s_geq w1 w2
	|_ -> raise (Invalid_argument "digest_bool geq"))
    |"leq" -> 
	(match get_app_args t with
	| [left;right] ->
	    let w1 = digest_int_term left ctx in
	    let w2 = digest_int_term right ctx in
	    s_leq w1 w2
	|_ -> raise (Invalid_argument "digest_bool leq"))
    |"lt" -> 
	(match get_app_args t with
	| [left;right] ->
	    let w1 = digest_int_term left ctx in
	    let w2 = digest_int_term right ctx in
	    s_lt w1 w2
	|_ -> raise (Invalid_argument "digest_bool lt"))
    |"gt" -> 
	(match get_app_args t with
	| [left;right] ->
	    let w1 = digest_int_term left ctx in
	    let w2 = digest_int_term right ctx in
	    s_gt w1 w2
	|_ -> raise (Invalid_argument "digest_bool gt"))
    |"IF" -> (match get_app_args t with
      | [pred;left;right] -> 
	  let w1 = digest_bool_term pred ctx in
	  let w2 = digest_bool_term left ctx in
	  let w3 = digest_bool_term right ctx in
	  s_and (s_imp w1 w2) (s_imp (s_not w1) w3)
      | _ -> raise (Invalid_argument "digest_bool IF"))
    |_-> digest_default t ctx)
    end
  else 
    begin
      debug_pr_newline "sformula" "trying misc";
      digest_default t ctx
    end
   ) 
	
and digest_binding name (VarBind(ty,tm)) ctx=
  debug_wrapper "sformula" 
    (fun () -> print_str_tm ("digest_binding:  name "^name^" type ") ctx ty )
    (fun str -> pr ("digest_binding result: "^str)) 
    (fun () -> 
      let ty = eval_type ctx ty in 
  match (ty,tm) with
  |(TmArrow(_,_,_,_),None) -> s_true()
  | (TmVar(fi,n,_),_) -> 
      (match get_binding fi ctx n with
      | VarBind(kind,Some ty2) -> digest_binding name (VarBind(ty2,tm)) ctx
      |_ -> digest_default ty ctx)
  |_ ->
      if is_primitive ty (*&& is_full_prim_app dummyinfo ctx ty*)
      then
	match (prim_name ty, tm) with
	|("*",_) -> s_true()
	|("Int",None) -> s_true() (* s_eq name (s_add name (s_num 0)) *)
	|("Bool",None) -> s_true() (* s_or name (s_not name) *)
	|("Refine",None) -> 
	    let (x,r_ty,body) = get_refine_args ty in
	    let bind = (VarBind(r_ty,None)) in
	    let s_ty = digest_binding name bind ctx in
	    let s_tm = digest_bool_term body (add_binding ctx name bind) in
            s_and s_ty s_tm
	|("Int", Some t) ->
	    let s_ty = digest_binding name (VarBind(ty,None)) ctx in
	    let s_tm = digest_bool_term t ctx in
	    s_and (s_eq name s_tm) s_ty
	|("Bool",Some t) -> 
	    let s_ty = digest_binding name (VarBind(ty,None)) ctx in
	    let s_tm = digest_bool_term t ctx in
	    s_and (s_iff name s_tm) s_ty
	|(_,Some t) -> digest_binding name (VarBind(ty,None)) ctx
	|_ -> digest_default ty ctx
      else
	match (ty,tm) with
(*	|(_,Some t) -> digest_binding name (VarBind(ty,None)) ctx   cf-what does this do?*)
	|_ -> digest_default ty ctx
    ) 

let rec digest_context (ctx:context) = 
  if ctx = []
  then s_true()
  else
    let (name,bind) = List.hd ctx in
    let new_ctx = List.tl ctx in
    let s_1 = digest_binding name bind new_ctx in
    debug "sformula"
      (fun () ->
        pr ("Term for binding "^name^" is: "^s_1); Format.force_newline());
    let s_rest = digest_context new_ctx in
    s_1^" "^s_rest

let create_sformula (ctx:context) (t1:term) : (string * bool)=
  let ctx = List.map (fun (v,b) -> (get_fresh_simplify_name v,b)) ctx in
  debug "sformula" 
    (fun () -> 
      pr "create_sformula: ";
      print_ctx ctx;
      break();
      print_str_tm "|= " ctx t1
    );
  let old_global_pred = !global_pred in
  global_pred := "";
  let old_incomplete_translation = !incomplete_translation in
  incomplete_translation := false;
  (* vars := var_list_from_ctx ctx; UGLY HACK *)
  let s_ctx = digest_context ctx in
  let s_t1 = digest_bool_term t1 ctx in
  let cur_global_pred = !global_pred in
  global_pred := old_global_pred;
  let cur_incomplete_translation = !incomplete_translation in
  incomplete_translation := old_incomplete_translation;
  ((s_imp (s_and s_ctx cur_global_pred) s_t1), cur_incomplete_translation)
    
let prove (ctx:context) (t:term) : 'a option bool3 =
  if Options.nosimplify()
  then 
    Maybe
  else
    debug_wrapper "prover"
      (fun () -> 
        pr "prover: ";
        print_ctx ctx;
        break();
        print_str_tm "|= " ctx t)
      (fun r -> 
         pr "prover: ";
         print_ctx ctx;
         break();
         print_str_tm "|= " ctx t;
         pr ((match r with 
              | True -> "True" 
              | False _ -> "False" 
              | Maybe -> "Maybe")^"\n"))
      (fun () ->
         let (simplify_formula,incomplete_translation) = create_sformula ctx t in
        debug "sformula" (fun () -> pr ("Sending to simplify: "^simplify_formula^"\n"));
        match call_simplify simplify_formula with
        | True -> True
        | Maybe -> Maybe
        | False _ -> if incomplete_translation then Maybe else False None)
