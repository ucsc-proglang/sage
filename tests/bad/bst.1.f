let Rint (lo:Int) (hi:Int) :* = (x:Int. (and  (lo<=x)  (x<hi)));;

datatype BiTree (lo:Int) (hi:Int) =
  Empty 
| Node of (mid:(Rint lo hi)) (left:(BiTree lo mid)) (right:(BiTree mid hi));;


let rec search (lo:Int) (hi:Int) (t:BiTree lo hi) (v:Rint lo hi) : Bool =
  caseBiTree lo hi t Bool
    (fn u => true)
    (fn i l r =>
     (or (i=v) 
        (if[Bool] v < i
        then search lo i l v
        else search i hi r v)));;

let rec insert (lo:Int) (hi:Int) (t:BiTree lo hi) (v:Rint lo hi) : (BiTree lo hi) =
  caseBiTree lo hi t (BiTree lo hi)
    (fn u =>
       Node lo hi v (Empty lo v) (Empty v hi))
    (fn i l r =>
     if[(BiTree lo hi)] v < i /* err if i <= v  or  v <= i*/
     then Node lo hi i (insert lo i l /*err if r*/ (v/* err if i */+0)) r /* err if l */
     else Node lo hi i l (insert i hi r (v+0)));;

let rec sum (lo:Int) (hi:Int) (t:BiTree lo hi) : Int =
  caseBiTree lo hi t Int
    (fn u => 0)    
    (fn i l r =>
      (sum lo i l) + (sum i hi r));;



let MININT : Int = 0-32767;;
let MAXINT : Int = 32767;;

let BTree = BiTree MININT MAXINT;; 
let Int16 = Rint MININT MAXINT;;
let mt:BTree = Empty MININT MAXINT;;
let ins (t:BTree) (v:Int16) : BTree = insert MININT MAXINT t v;;
let get (t:BTree) (v:Int16) : Bool = search MININT MAXINT t v;;

let t : BTree = (ins (ins (ins mt 1) 2) 3);;


let Pos = Rint 1 MAXINT;;

let PTree = BiTree 1 MAXINT;; 

let mtP:PTree = Empty 1 MAXINT;;
let insP (t:PTree) (v:Pos) : PTree = insert 1 MAXINT t v;;
let getP (t:PTree) (v:Pos) : Bool = search 1 MAXINT t v;;

let tP : PTree = (insP (insP (insP mtP 1) 2) 3);; 

(getP tP 2);; /* true */
(getP tP 4);; /* false */


let sumAny = sum MININT MAXINT;;
sumAny t;;
/* sumAny tP;; */
