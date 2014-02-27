open Print
open Options

type key = string

let debug_on key = ((mem "all" !option_debug_keys) || (mem key !option_debug_keys))

let debug key f = 
  if debug_on key then f() else ()
    
let debug_pr key str =
  if debug_on key then (pr str) else ()

let debug_pr_newline key str =
  if debug_on key then (pr str; force_newline()) else ()

let counter = ref(0)

let debug_wrapper (id :key) (intro : unit ->unit) (concl : 'a -> unit)
    (fn : unit -> 'a) : 'a =
  if debug_on id
  then 
    begin
      let ind = !counter in
      counter := ind + 1;
      open_box 4;
      print_int ind; pr " >> ";
      intro();
      let result = fn() in
      close_box();
      (*      if (!counter - 1) = ind 
              then close_box();*)
      force_newline();
      print_int ind; pr " << ";
      concl result;
      print_cut();
      result
    end
  else fn()
    
