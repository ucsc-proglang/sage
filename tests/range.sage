let Rint (lo:Int) (hi:Int) :* = (x:Int. (and  (lo<=x)  (x<hi)));;

let y : (Rint 4 9) = 5;;
let z : (Rint 5 10) = (y+1);;

let p (l:Int) (h:Int) (x:Rint l h) : (Rint (l+1) (h+1)) = x + 1;;


datatype BiTree (lo:Int) (hi:Int) =
  Empty of (u:Unit)
| Node of (mid:(Rint lo hi)) (left:(BiTree lo mid)) (right:(BiTree mid hi));;


datatype SortedList (y:Int)= 
  EL of (u:Unit)
| NL of (x:Int.y<=x) (rest:SortedList x);;


let rec listify (lo:Int) (hi:Int.lo<hi) (t:BiTree lo hi) (list:SortedList hi) : (SortedList lo) =
  caseBiTree lo hi t (SortedList lo)
    (fn u => list)
    (fn i l r => 
      NL lo i (listify i (hi + 0) r list)
      );;


