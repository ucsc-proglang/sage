/* standard */

let Pair = fn (X:*) (Y:*) :* => (Z:*->(X->Y->Z)->Z);;

let pair = fn (X:*) (Y:*) (x:X) (y:Y) : Pair X Y => fn (Z:*) (f:X->Y->Z) => f x y;;
   
let Sum = fn (X:*) (Y:*) => (Z:*->(X->Z)->(Y->Z)->Z);;
let inl = fn (X:*) (Y:*) (x:X) : (Sum X Y) => fn (Z:*) (fx:X->Z) (fy:Y->Z) => fx x;;
let inr = fn (X:*) (Y:*) (y:Y) : (Sum X Y) => fn (Z:*) (fx:X->Z) (fy:Y->Z) => fy y;;
   
/* basically a datatype declaration */

let rec Tree:* = Sum Int (Pair Tree Tree);; 
let Node = Pair Tree Tree;;

let leaf(n:Int) =
  inl Int Node n;;
let node(a:Tree) (b:Tree): Tree =
  inr Int Node (pair Tree Tree a b);;
let case(t:Tree) (Z:*) (f1:Int->Z) (f2:Tree->Tree->Z) :Z =
  (t as (Sum Int Node)) 
     Z 
     f1 
     (fn (p:Node):Z => p Z f2);;

/* code to manipulate trees */
  
let t1 = leaf 3;;
let t2 = node t1 t1;;

let rec size(t:Tree):Int = 
  (case t Int
     (fn n => 1)
     (fn c1 c2 => (size c1) + (size c2)));;

(size t2);;

let rec sum(t:Tree):Int = 
  (case t Int
     (fn n => n)
     (fn c1 c2 => (sum c1) + (sum c2)));;

(sum t2);;


