/*
vim:syntax=sml
*/

let MININT = 0 - 32768;;
let MAXINT = 32767;;

datatype List (lo:Int) =
  Null
| Cons of (h:Int) (t:(List lo));;

datatype Pair (S:*) (T:*) =
  MkPair of (fst:S) (snd:T);;

/* (OrderedList n) <: (OrderedList m) iff n >= m */
datatype OrderedList (lo:Int) =
  ONull
| OCons of (h:Int.(lo <= h)) (t:(OrderedList h));;

let l : (OrderedList 0) = OCons 0 0 (OCons 1 1 (ONull 2));;

let exact (d:Int) : (e:Int.e=d) = d+0;;

let min (a:Int) (b:Int) : (c:Int.(a>=c) && (b>=c)) =
  if[(c:Int.(a>=c) && (b>=c))] (a >= b) 
  then exact b
  else exact a;;

let Above (x:Int):* = (y:Int.x<=y);;

let rec merge (lo:Int)
              (lo1:Above lo)
              (l1:OrderedList lo1)
              (lo2:Above lo)
              (l2:OrderedList lo2) :
              (OrderedList lo) =
  caseOrderedList lo1 l1 (OrderedList lo)
    (fn u => l2)
    (fn h1 t1 =>
      caseOrderedList lo2 l2 (OrderedList lo)
        (fn u => l1)
        (fn h2 t2 =>
          if[OrderedList lo] h1 < h2 then
            (OCons lo h1
              (merge h1 h1 t1 h2 (OCons h2 h1 t2))) /* BUG: flipped args */
          else
            (OCons lo h2
              (merge h2 h2 t2 h1 (OCons h1 h1 t1)))
        )
    )
;;

let rec length (lo:Int) (l:(List lo)) : Int =
  caseList lo l Int
    (fn u => 0)
    (fn h t => 1 + (length lo t));;

let partition (lo:Int) (l:(List lo)) : (Pair (List lo) (List lo)) =
  let rec helper (firsthalf:(List lo)) (secondhalf:(List lo)) :
                 (Pair (List lo) (List lo)) =
    caseList lo firsthalf (Pair (List lo) (List lo))
      (fn u => (MkPair (List lo) (List lo) firsthalf secondhalf))
      (fn h t =>
          if[Pair (List lo) (List lo)]
            ((length lo t) >= ((length lo secondhalf) + 1))
          then
            helper t (Cons lo h secondhalf)
          else
            MkPair (List lo) (List lo) firsthalf secondhalf)
  in
    helper l (Null lo)
;;


let rec merge_sort (lmin:Int) (l:(List lmin)) : (OrderedList lmin) =
  casePair (List lmin) (List lmin) (partition lmin l) (OrderedList lmin)
    (fn fst snd =>
      let sorted1 = merge_sort lmin fst in
      let sorted2 = merge_sort lmin snd in
        merge lmin lmin sorted1 lmin sorted2);;
