let rec fib (x:Int.x >= 1) =
  IF (Unit->Int)
     (x <= 2)
     (fn u => 1)
     (fn u => (fib (x-1)) + (fib (x-2)))
     unit;;

fib 1;;
fib 2;;
fib 3;;
