(* 
   The lexical analyzer: lexer.ml is generated automatically
   from lexer.mll.
   
   The only modification commonly needed here is adding new keywords to the 
   list of reserved words at the top.  
*)

{
open Support.Error
open Parser

let reserved_words = [
  
  (* Core Keywords *)
  ("lambda", fun i -> Parser.LAMBDA i);
  ("let", fun i -> Parser.LET i);
  ("in", fun i -> Parser.IN i);
  ("as", fun i -> Parser.AS i);
  
  (* Added for syntactic sugar *)
  ("fun", fun i -> Parser.LAMBDA i);
  ("fn", fun i -> Parser.FN i);
  ("rec", fun i -> Parser.REC i);
  ("if", fun i -> Parser.IF i);
  ("then", fun i -> Parser.THEN i);
  ("else", fun i -> Parser.ELSE i);
  ("assume", fun i -> Parser.ASSUME i);
  ("assumenot", fun i -> Parser.ASSUMENOT i);
  ("datatype", fun i -> Parser.DATATYPE i);
  ("of", fun i -> Parser.OF i);
  (*("case", fun i -> Parser.CASE i);
  ("of", fun i -> Parser.OF i);*)
  
  (* Symbols *)
  ("+", fun i -> Parser.PLUS i);
  ("-", fun i -> Parser.MINUS i);
(*  ("_", fun i -> Parser.USCORE i); *)
  ("'", fun i -> Parser.APOSTROPHE i);
  ("\"", fun i -> Parser.DQUOTE i);
  ("!", fun i -> Parser.BANG i);
  ("#", fun i -> Parser.HASH i);
  ("$", fun i -> Parser.TRIANGLE i);
  ("*", fun i -> Parser.STAR i);
  ("|", fun i -> Parser.VBAR i);
  (".", fun i -> Parser.DOT i);
  (";", fun i -> Parser.SEMI i);
  (";;", fun i -> Parser.SEMISEMI i);
  (",", fun i -> Parser.COMMA i);
  ("/", fun i -> Parser.SLASH i);
  (":", fun i -> Parser.COLON i);
  ("::", fun i -> Parser.COLONCOLON i);
  (":-", fun i -> Parser.COLONDASH i);
  ("=", fun i -> Parser.EQ i);
  ("==", fun i -> Parser.EQEQ i);
  ("<=>", fun i -> Parser.IFF i);
  ("[", fun i -> Parser.LSQUARE i); 
  ("<", fun i -> Parser.LT i);
  ("<=", fun i -> Parser.LEQ i);
  ("<:", fun i -> Parser.SUBTYPE i);
  ("{", fun i -> Parser.LCURLY i); 
  ("(", fun i -> Parser.LPAREN i); 
  ("<-", fun i -> Parser.LEFTARROW i); 
  ("{|", fun i -> Parser.LCURLYBAR i); 
  ("[|", fun i -> Parser.LSQUAREBAR i); 
  ("}", fun i -> Parser.RCURLY i);
  (")", fun i -> Parser.RPAREN i);
  ("]", fun i -> Parser.RSQUARE i);
  (">", fun i -> Parser.GT i);
  (">=", fun i -> Parser.GEQ i);
  ("|}", fun i -> Parser.BARRCURLY i);
  ("|>", fun i -> Parser.BARGT i);
  ("|]", fun i -> Parser.BARRSQUARE i);
  ("&&", fun i -> Parser.AND i);
  ("||", fun i -> Parser.OR i);
  ("?", fun i -> Parser.QUESTION i);

  (* Special compound symbols: *)
  (":=", fun i -> Parser.COLONEQ i);
  ("->", fun i -> Parser.ARROW i);
  ("=>", fun i -> Parser.DARROW i);
  ("==>", fun i -> Parser.DDARROW i);
]

(* Support functions *)

type buildfun = info -> Parser.token
let (symbol_table : (string, buildfun) Hashtbl.t) = Hashtbl.create 1024
let _ =
  List.iter (fun (str, f) -> Hashtbl.add symbol_table str f) reserved_words

let create_id i str =
  try (Hashtbl.find symbol_table str) i
  with _ ->
    if (String.get str 0) >= 'A' && (String.get str 0) <= 'Z' && false then
       Parser.ID {i=i;v=str}
    else 
       Parser.ID {i=i;v=str}

let lineno   = ref 1
and depth    = ref 0
and start    = ref 0

and filename = ref ""
and start_lex = ref dummyinfo

let create in_file stream =
  if not (Filename.is_implicit in_file) then filename := in_file
  else filename := Filename.concat (Sys.getcwd()) in_file;
  lineno := 1; start := 0; Lexing.from_channel stream

let newline lexbuf = incr lineno; start := (Lexing.lexeme_start lexbuf)

let info lexbuf =
  create_info (!filename) (!lineno) (Lexing.lexeme_start lexbuf - !start + 1)

let text = Lexing.lexeme

let string_buffer = ref (String.create 2048)
let string_end = ref 0

let reset_str () = string_end := 0

let add_str ch =
  let x = !string_end in
  let buffer = !string_buffer
in
  if x = String.length buffer then
    begin
      let new_buffer = String.create (x*2) in
      String.blit buffer 0 new_buffer 0 x;
      String.set new_buffer x ch;
      string_buffer := new_buffer;
      string_end := x+1
    end
  else
    begin
      String.set buffer x ch;
      string_end := x+1
    end

let get_str () = String.sub (!string_buffer) 0 (!string_end)

let extractLineno yytext offset =
  int_of_string (String.sub yytext offset (String.length yytext - offset))
}


(* The main body of the lexical analyzer *)

rule main = parse
  [' ' '\009' '\012']+     { main lexbuf }

| [' ' '\009' '\012']*("\r")?"\n" { newline lexbuf; main lexbuf }

| "*/" { error (info lexbuf) "Unmatched end of comment" }

| "/*" { depth := 1; start_lex := info lexbuf; comment lexbuf; main lexbuf }

| "# " ['0'-'9']+
    { lineno := extractLineno (text lexbuf) 2 - 1; get_file lexbuf }

| "# line " ['0'-'9']+
    { lineno := extractLineno (text lexbuf) 7 - 1; get_file lexbuf }

| ['0'-'9']+
    { Parser.INTV{i=info lexbuf; v=int_of_string (text lexbuf)} }

| ['0'-'9']+ '.' ['0'-'9']+
    { Parser.FLOATV{i=info lexbuf; v=float_of_string (text lexbuf)} }

| ['A'-'Z' 'a'-'z' '_']
  ['A'-'Z' 'a'-'z' '_' '0'-'9' '\'']*
    { create_id (info lexbuf) (text lexbuf) }

| ":=" | "<:" | "<-" | "->" | "=>" | "==>" | "::" | ":-"
| "{|" | "|}" | "<|" | "|>" | "[|" | "|]" | "==" | ";;"
| "<=" | ">=" | "<=>" | "<:" | "&&" | "||"
    { create_id (info lexbuf) (text lexbuf) }

| ['~' '%' '\\' '+' '-' '&' '|' ':' '@' '`' '$']+
    { create_id (info lexbuf) (text lexbuf) }

| ['*' '#' '/' '?' '!' '^' '(' ')' '{' '}' '[' ']' '<' '>' '.' ';' '_' ','
   '=' '\'' ';']
    { create_id (info lexbuf) (text lexbuf) }

| "\"" { reset_str(); start_lex := info lexbuf; string lexbuf }

| eof { Parser.EOF(info lexbuf) }

| _  { error (info lexbuf) "Illegal character" }

and comment = parse
  "/*"
    { depth := succ !depth; comment lexbuf }
| "*/"
    { depth := pred !depth; if !depth > 0 then comment lexbuf }
| eof
    { error (!start_lex) "Comment not terminated" }
| [^ '\n']
    { comment lexbuf }
| "\n"
    { newline lexbuf; comment lexbuf }

and get_file = parse
  " "* "\"" { get_name lexbuf }

and get_name = parse
  [^ '"' '\n']+ { filename := (text lexbuf); finish_name lexbuf }

and finish_name = parse
  '"' [^ '\n']* { main lexbuf }

and string = parse
  '"'  { Parser.STRINGV {i = !start_lex; v=get_str()} }
| '\\' { add_str(escaped lexbuf); string lexbuf }
| '\n' { add_str '\n'; newline lexbuf; string lexbuf }
| eof  { error (!start_lex) "String not terminated" }
| _    { add_str (Lexing.lexeme_char lexbuf 0); string lexbuf }

and escaped = parse
  'n'    { '\n' }
| 't'    { '\t' }
| '\\'   { '\\' }
| '"'    { '\034'  }
| '\''   { '\'' }
| ['0'-'9']['0'-'9']['0'-'9']
    {
      let x = int_of_string(text lexbuf) in
      if x > 255 then
        error (info lexbuf) "Illegal character constant"
      else
        Char.chr x
    }
| [^ '"' '\\' 't' 'n' '\'']
    { error (info lexbuf) "Illegal character constant" }

(*  *)
