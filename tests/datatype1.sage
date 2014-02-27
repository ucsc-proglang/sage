datatype D = 
      Moo of (y:Bool) (z:Bool)
    | Cow of (z:Int);;

/* This should be equivalent to the following: */

/* let rec W : * =  (Z:* -> (Bool->Z) -> (Int -> Z) -> (Unit -> Z) -> Z);; */

/* let rec DC (x:Unit) :* = (Z:* -> (Bool->Z) -> (Int -> Z) -> (Unit -> Z) -> Z);;
let D = (DC unit);; */

/*
let rec D : * =  (Z:* -> (Bool->Z) -> (Int -> Z) -> (Unit -> Z) -> Z);; 

let Moo = fn (d:Bool) : D => fn (Z:*) (f1:Bool->Z) (f2:Int->Z) (f3:Unit->Z) => f1 d;;
let Cow = fn (d:Int): D => fn (Z:*) (f1:Bool->Z) (f2:Int->Z) (f3:Unit->Z)  => f2 d;;
let Moose : D = fn (Z:*) (f1:Bool->Z) (f2:Int->Z) (f3:Unit->Z)  => f3 unit;;

let caseD = fn (Z:*) (v:D) (f1:Bool->Z) (f2:Int->Z) (f3:Unit->Z) => (v Z) f1 f2 f3;;

let moo = fn (d:D) : Int =>
  caseD Int d (fn x => if[Int] x then 10 else 100) (fn x => x) (fn x => 2);;
*/
