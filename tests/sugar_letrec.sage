let rec fib (x:Int) : Int = 
	if [Int] (x <= 1) 
	then 1
	else (fib (x-1)) + (fib (x-2));;

fib 3;;
fib 4;;
fib 5;;
fib 6;;
fib 7;;

/*let rec fib (x:Int) : Int = 
	if [Int] (x <= 1) 
	then (fn (x:Bool) => x) true
	else (fib (x-1)) + (fib (x-2));;
*/
