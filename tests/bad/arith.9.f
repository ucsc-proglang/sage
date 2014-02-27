/*
vim:syntax=sml
*/

let min (a:Int) (b:Int) =
  if[c:Int.(a>=c) && (b>=c)] a >= b then
    b
  else
    a;;

let max (a:Int) (b:Int) =
  if[c:Int.(c>=a) && (c>=b)] a >= b then
    a
  else
    b;;

let abs (x:Int) =
  if[r:Int.r>=0] x >= 0 then
    x
  else
    (0 - x);;

let Pos = (x:Int. x > 0);;

let rec rem (n:Int) (d:Pos) : (r:Int.(r <= n) && (r < d)) =
  if[r:Int.(r <= n) && (r < d)] n < d then
    n
  else
    rem (n - d) d;;

let pred (x:Int) : (r:Int.r < x) = x - 1;;

let succ (x:Int) : (r:Int.r < x) = x + 1;;

abs (0 - 5);;

min 7 22;;

max 12 41;;

rem 6 4;;

pred 18;;

succ 9;;
