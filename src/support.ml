open Format

let foldi_left f init arr =
  snd (List.fold_left
         (fun (i, accum) next -> i + 1, f i accum next)
         (0,init)
         arr)

let mapi f li =
  List.rev (
    foldi_left
      (fun i newlist elem -> (f i elem) :: newlist)
      []
      li
  )

let countlist l =
  let rec countlist' l c =
    match l with
    | [] -> []
    | l::ls -> c::(countlist' ls (c + 1))
  in
    countlist' l 0

(* tail recursive to use fold_left + rev instead of fold_right, but
   mostly irrelevant *)
let rec split3 l =
  let li1, li2, li3 =
    List.fold_left
      (fun (li1,li2,li3) (e1, e2, e3) -> (e1::li1, e2::li2, e3::li3))
      ([],[],[])
      l
  in
  List.rev li1, List.rev li2, List.rev li3

let is_numeric_char chr =
  match chr with
  | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' -> true
  | _ -> false

let rec list_of_string str =
  if String.length str == 0 then []
  else (String.get str 0)::
       (list_of_string (String.sub str 1 ((String.length str) - 1)))

let is_numeric str =
  let all_digits cs =
    List.fold_left (fun a b -> a && b) true
      (List.map is_numeric_char cs) in
  let chars = (list_of_string str) in
  match chars with
  | ('-'::cs) -> all_digits cs
  | _ -> all_digits chars


module Error = struct

exception Exit of int

type info = FI of string * int * int | UNKNOWN
type 'a withinfo = {i: info; v: 'a}

let dummyinfo = UNKNOWN
let create_info f l c = FI(f, l, c)

let string_of_info fi = match fi with
  | FI(f, l, c) -> f ^ ":" ^ (string_of_int l) ^ "." ^ (string_of_int c)
  | UNKNOWN -> "<Unknown file and line>"

let filename_of_info fi = match fi with
  | FI(f, l, c) -> f
  | UNKNOWN -> "<Unknown file and line>"

let lineno_of_info fi = match fi with
  | FI(f, l, c) -> l
  | UNKNOWN -> -1

let column_of_info fi = match fi with
  | FI(f, l, c) -> c
  | UNKNOWN -> -1

let errf f =
  print_flush();
  open_vbox 0;
  open_hvbox 0; f(); print_cut(); close_box(); print_newline();
  raise (Exit 1)

let print_info =
  (* In the text of the book, file positions in error messages are replaced
     with the string "Error:" *)
  function
    FI(f, l, c) ->
      force_newline();
      print_string f;
      print_string ":";
      print_int l; print_string ".";
      print_int c; print_string ":"
  | UNKNOWN ->
      force_newline();
      print_string "<Unknown file and line>: "

let errf_at fi f =
  errf (fun () ->
          print_string "Error: ";
          print_info fi;
          print_space();
          f())

let err s =
  errf (fun () -> print_string "Error: "; print_string s; print_newline())

let error fi s = errf_at fi (fun()-> print_string s; print_newline())

let warnings = ref 0

let num_warnings() = !warnings

let warnf f =
  warnings := !warnings + 1;
  print_flush();
  open_vbox 0;
  open_hvbox 0; f(); print_cut(); close_box(); print_newline()

let warnf_at fi f =
  warnf (fun () ->
           print_string "Warning: ";
           print_info fi;
           print_space();
           f())

let warn s =
  warnf (fun()-> print_string "Warning: "; print_string s; print_newline())

let warning fi s = warnf_at fi (fun()-> print_string s; print_newline())

end

(* ---------------------------------------------------------------------- *)

module Pervasive = struct

  type 'a bool3 = True | False of 'a | Maybe
  type info = Error.info

open Str
let newline = regexp "\n"
let pr s =
  let substrs = full_split newline s in
  List.iter
    (function Delim _ -> force_newline () | Text s -> Format.print_string s)
    substrs

end (* module pervasive *)
