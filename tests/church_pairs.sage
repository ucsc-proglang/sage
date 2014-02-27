/* Creating data types without using syntactic sugar */
let Pair = fn (X:*) (Y:*) => (Z:*->(X->Y->Z)->Z);;
let pair = fn (X:*) (Y:*) => fn (x:X) (y:Y) (Z:*) => fn (f:X->Y->Z) => f x y;;
let fst = fn (X:*) (Y:*) => fn (p:Pair X Y) => p X (fn (x:X) (y:Y) => x);;
let snd = fn (X:*) (Y:*) => fn (p:Pair X Y) => p Y (fn (x:X) (y:Y) => y);;

