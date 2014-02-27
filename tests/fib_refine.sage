let Pos = (x:Int. x>=0);;

let rec fib (x:Pos) : Pos =
  if[Pos] x < 2 then
    1
  else
    (fib (x-1)) + (fib (x-2));;

fib 3;;

