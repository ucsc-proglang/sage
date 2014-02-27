open Sys
open Db
open Format

let main () =
  Arg.parse []
     (fun s ->
          if (file_exists s) then
          let db = (new database s) in
          db#init();
          db#print_state()
          else
          print_string "DB does not exist"; force_newline()
     ) ""

let () = set_max_boxes 1000
let () = set_margin 120
let () = main()
