/* From Putting Curry-Howard to Work, the Term example of sec 4,
   dropping the Pair constructor for simplicity.
*/

let rec Term (X:*) : * =
  R:*
  -> (X -> R)
  -> (Y:* -> (Term (Y->X)) -> (Term Y) -> R)
  -> R
;;

let Const (X:*) (x:X) : Term X = 
   (fn (R:*) (c:X->R) (a:(Y:* -> (Term (Y->X)) -> (Term Y) -> R)) => 
        c x)
;;

let App (X:*) (Y:*) (f:Term (Y->X)) (y:Term Y) : Term X =
   (fn (R:*) (c:X->R) (a:(Y:* -> (Term (Y->X)) -> (Term Y) -> R)) => 
        a Y f y)
;;
    
let t:Term Int = 
  App Int Int 
    (Const (Int->Int) (fn (x:Int) => x))
    (Const Int 4)
;;

let rec eval (X:*) (t:Term X) : X =
  t X
  /* Const */
  (fn (x:X) => x)
  /* App */
  (fn (Y:*) (t1:Term (Y->X)) (t2:Term Y) =>
     (eval (Y->X) t1) (eval Y t2))
;;

eval Int t
;;
