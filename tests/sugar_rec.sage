rec f x => x;;
(rec f x => x) 3;;
(rec f (x:Int) : Int =>
  IF Int (x = 0) (fn u => x) (fn u => x + (f (x-1)))) 3;;

let fib = rec f (x:Int) : Int => 
	if [Int] (x <= 1) then
		1
	else
		(f (x-1)) + (f (x-2));;

fib 3;;
fib 4;;
fib 5;;
fib 6;;
fib 7;;
