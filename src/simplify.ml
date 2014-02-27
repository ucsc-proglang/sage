open Unix
open Buffer
open Str
open Options
open Support.Error
open Support.Pervasive
open Print

let stdin_write = ref(out_channel_of_descr stdout)
and stdout_read = ref(in_channel_of_descr stdin)
and simplify_on = ref(false)

(****  logging utilities  ****)
let counter = ref 0

let next_log_file() =
  let name = "sexp." ^ (string_of_int !counter) ^ ".log" in
  counter := !counter + 1;
  name

let log log_file str =
  match (log_file) with
  | None -> ()
  | Some(file) ->
      output_string file str;
      output_string file "\n\n";
      flush file

let close_log log_file =
  match (log_file) with
  | None -> ()
  | Some(file) -> close_out file

(********)

let stop_simplify () =
  (ignore (close_process (!stdout_read, !stdin_write)))

let write_to_simplify formula =
  output_string !stdin_write (formula ^ "\n");
  flush !stdin_write

let start_simplify () =
  putenv "PROVER_KILL_TIME" (string_of_int (Options.simplifytimeout()));
  let cmd = Options.prover() ^ " " ^ Options.prover_options() in
  pr "Starting Simplify with Command: ";
  pr cmd;
  Format.force_newline();
  let (read_simplify, write_simplify) =
    open_process cmd in
  stdin_write := write_simplify;
  stdout_read := read_simplify;
  simplify_on := true;
  Options.at_exit stop_simplify;
  write_to_simplify Formulaparse.bool_term_rules

let send_to_simplify formula =
  if not (!simplify_on)
  then start_simplify ();
  write_to_simplify formula

let parse_answer res =
  try
    ignore (search_forward (regexp (quote "Valid.")) res 0);
    True
  with
    Not_found ->
      try
        let rng_end = search_forward (regexp (quote "Invalid.")) res 0 in
        try
          let rng_beg =
            (search_forward (regexp (quote "context:")) res 0) + 7
          in
          let counterexample = String.sub res rng_beg (rng_end-rng_beg) in
          False (Some counterexample)
        with
          Not_found -> False None
      with
        Not_found -> err "Bad answer from Simplify - check Simplify.out!"

let rec get_simplify_response () =
  let response = input_line !stdout_read in
  if (String.length response) = 0
  then get_simplify_response()
  else response

let num_calls = ref 0

let () = Options.at_exit (fun () ->
  pr(Printf.sprintf "Number of Simplify queries: %d\n" !num_calls))

let call_simplify formula =
  num_calls := !num_calls+1;
  let log_file = if (Options.slog()) then
    Some(open_out (next_log_file()))
  else
    None
  in
  log log_file formula;
  send_to_simplify formula;
  let response = get_simplify_response() in
  log log_file (";; " ^ response);
  close_log log_file;
  parse_answer response
    
