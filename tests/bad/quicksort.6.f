datatype Pair (X:*) (Y:*) = 
      Pcons of (x:X) (y:Y);;

let LTInt (hi:Int) :* = (x:Int. (x <= hi));;

let RInt (hi:Int)  (lo:(LTInt hi)):* = (x:Int. (and  (lo<=x)  (x<hi)));;

/*let exact (d:Int) : (RInt (d+1) (d+0)) = cast Int (RInt (d+1) (d+0)) d;;*/


datatype RangeList (max:Int) (min:(LTInt max)) = 
  RNull of (u:Unit)
| RCons of (hd:(RInt max min)) (tl:RangeList max min);;

datatype SortedList (max:Int) (min:(LTInt max)) = 
  SNull of (u:Unit)
| SCons of (hd:(RInt max min)) (tl:(SortedList max hd));;

let rec sortAppend (ub:Int) (mid:(LTInt ub)) (lb:(LTInt mid))
      (l1:SortedList mid lb) (l2:SortedList ub mid):
      (SortedList ub lb) =
      caseSortedList mid lb l1 (SortedList ub lb)
        (fn u => l2)
        /* BUG: switched ub and mid */
        (fn x y => SCons ub lb x (sortAppend mid ub x y l2));;

let rec partition (ub:Int) (lb:(LTInt ub)) (pivot:(RInt ub lb)) 
      (l:(RangeList ub lb)) (l1:(RangeList pivot (lb+0)))
      (l2:(RangeList ub pivot)):
      (Pair (RangeList pivot (lb+0)) (RangeList ub pivot)) =
      caseRangeList ub lb l 
      (Pair (RangeList pivot (lb+0)) (RangeList ub pivot))
      (fn u => (Pcons (RangeList pivot (lb+0)) (RangeList ub pivot) l1 l2))
      (fn x y =>
         if [(Pair (RangeList pivot (lb+0)) (RangeList ub pivot))] x >= pivot
         then (partition ub lb pivot y l1 (RCons ub pivot (x+0) l2))
         else (partition ub lb pivot y (RCons pivot (lb+0) (x+0) l1) l2));;

let rec quicksort (ub:Int) (lb:(LTInt ub)) (l:(RangeList ub lb)): 
      (SortedList ub lb) =
      caseRangeList ub lb l (SortedList ub lb)
      (fn u1 => (SNull ub lb unit))
      (fn x1 y1 => 
           (caseRangeList ub lb y1 (SortedList ub lb)
           (fn u2 => (SCons ub lb x1 (SNull ub x1 unit)))
           (fn x2 y2 =>
                (let pivot = x1 in 
                (let pairLists = 
                   (partition ub lb pivot y1 
                   (RNull pivot (lb+0) unit) (RNull ub pivot unit)) in
                (casePair (RangeList pivot (lb+0)) (RangeList ub pivot)
                          pairLists (SortedList ub lb)
                (fn x y => (sortAppend ub pivot (lb+0) 
                            (quicksort pivot (lb+0) x)
                            (SCons ub pivot (pivot+0) (quicksort ub pivot y))))
                            ))))));;

