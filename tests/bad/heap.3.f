/*
vim:syntax=sml
*/

/* (Heap m) <: (Heap n) iff n <= m */
datatype Heap (min:Int) = 
      Empty
    | Node of (x:Int. min <= x) (c:Int) (l:(Heap x)) (r:(Heap x));;

let n1 : (Heap 0) = (Empty 0);;
let n2 : (Heap 1) = Node 1 1 6
                       (Node 1 2 2 (Empty 2) (Empty 2))
                       (Node 1 4 2 (Empty 4) (Empty 4));;


let foo (h:(Heap 0)) : Bool = true;;

foo n2;;

let min (a:Int) (b:Int) : (c:Int.(a>=c) && (b>=c)) =
  if[(c:Int.(a>=c) && (b>=c))] (a >= b) then
    b
  else
    a;;

let max = fn (a:Int) (b:Int) =>
  if[c:Int.(c>=a) && (c>=b)] (a >= b) then
    a
  else
    b;;

let heap_children (min:Int) (h:(Heap min)) : Int =
  caseHeap min h Int
    (fn u => 0)
    (fn x c l r => c);;

let rec insert (hmin:Int)
               (h:(Heap hmin))
               (v:Int)
               (rmin:Int.(rmin <= hmin) && (rmin <= v)) :
               (Heap rmin) =
  caseHeap hmin h (Heap rmin)
    (fn u =>
      Node v v 2 (Empty v) (Empty v))
    (fn x c l r =>
      if[Heap rmin] v < x 
      then
        let newchildren : Int = (heap_children hmin h) + 1 in
        /* BUG: wrong arg */
        (Node hmin v newchildren (Node v x c l r) (Empty v))
      else /* v >= x */
        let lchildren : Int = heap_children x l in
        let rchildren : Int = heap_children x r in
        let newmin : (m:Int.(m <= x) && (m <= rmin)) = min x rmin in
        if[Heap rmin] lchildren < rchildren
        then
          Node rmin x (c + 1) (insert x l v x) r
        else
          Node rmin x (c + 1) l (insert x r v x))
;;

let extract_min (min:Int) (h:(Heap min)) : (r:Int.(min <= r)) =
  caseHeap min h (r:Int.(min <= r))
    (fn u => min) /* This seems weird */
    (fn x c l r => x)
;;

let h1 = insert 0 (Empty 0) 1 0;;
let h2 = insert 0 h1 2 0;;
extract_min 0 h2;;
