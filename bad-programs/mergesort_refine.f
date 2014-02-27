let Pair = fn (X:*) (Y:*) :* => (Z:*->(X->Y->Z)->Z);;

let pair = fn (X:*) (Y:*) (x:X) (y:Y) : Pair X Y => fn (Z:*) (f:X->Y->Z) => f x y;;
let fst = fn (X:*) (Y:*) (p:Pair X Y) : X => p X (fn (x:X) (y:Y) => x);;
let snd = fn (X:*) (Y:*) (p:Pair X Y) : Y => p Y (fn (x:X) (y:Y) => y);;
   
let Sum = fn (X:*) (Y:*) => (Z:*->(X->Z)->(Y->Z)->Z);;
let inl = fn (X:*) (Y:*) (x:X) : (Sum X Y) => fn (Z:*) (fx:X->Z) (fy:Y->Z) => fx x;;
let inr = fn (X:*) (Y:*) (y:Y) : (Sum X Y) => fn (Z:*) (fx:X->Z) (fy:Y->Z) => fy y;;
   
let rec IntList:* = Sum Unit (Pair Int IntList);;
let Cons = Pair Int IntList;;

let null = (inl Unit Cons unit) as IntList;;
let cons = fn (a:Int) (d:IntList) : IntList => 
   inr Unit Cons (pair Int IntList a d);;
let car (c:Cons) = fst Int IntList c;;
let cdr (c:Cons) = snd Int IntList c;;
let case = fn (l:IntList) (Z:*) (f1:Unit->Z) (f2:Cons->Z) :Z =>
   (l as (Sum Unit Cons)) Z f1 f2;;

/* Here is where the content starts.  For the sake of
   demonstration, I only sort positive ints. */

let is_sorted (L:IntList) : Bool =
    let rec helper (L:IntList) (lowbound:Int) : Bool =
        case L Bool
            (fn (u:Unit) => true)
            (fn (c:Cons) =>
                let elem = car c in
                (IF (Unit->Bool)
                    (elem >= lowbound)
                    (fn d => helper (cdr c) elem)
                    (fn d => false)
                    unit))
    in
    helper L 0
;;

let rec print_list (l:IntList) : Unit =
    case l Unit
        (fn (u:Unit) => unit)
        (fn (c:Cons) => let u = print (car c) in print_list (cdr c));;

let Pos = (x:Int.x>=0);;

let rec length (l:IntList) : Pos = 
  (case l Pos
     (fn (u:Unit) => 0)
     (fn (l2:Cons) => 1 + (length (cdr l2))));;

/* Uses crazy IF because cast algorithm fails */
let split_in_half (L:IntList) : (Pair IntList IntList) =
    let mkpair = pair IntList IntList in
    let rec helper (firsthalf:IntList) (secondhalf:IntList) :
                                       (Pair IntList IntList) =
        case firsthalf (Pair IntList IntList)
            (fn (u:Unit) => mkpair firsthalf secondhalf)
            (fn (c:Cons) =>
                let tl1 = cdr c in
                IF 
                    (Unit -> (Pair IntList IntList))
                    ((length tl1) >= ((length (secondhalf)) + 1))
                    (fn u => helper tl1 (cons (car c) secondhalf))
                    (fn u => mkpair firsthalf secondhalf)
                    unit)
    in
    helper L null
;;
        
let rec merge (L1:IntList) (L2:IntList) 
        : (l:IntList.(length l) = (length L1) + (length L2)) 
        =
    case L1 IntList
        (fn (u:Unit) => L2)
        (fn (c1:Cons) =>
            case L2 IntList
                (fn (u:Unit) => L1)
                (fn (c2:Cons) =>
                    let e1 = car c1 in    let e2 = car c2 in
                    IF (Unit -> IntList) (e1 >= e2)
                    (fn u => cons e2 (merge L1 (cdr c2)))
                        (fn u => cons e1 (merge (cdr c1) L2))
                        unit));;

let rec mergesort (L:IntList) : (l:IntList. is_sorted l) =
    let u = print_list L in
    let u = newline unit in
    IF (Unit -> IntList) (1 >= (length L))
        (fn u => L)
        (fn u => 
            let twolists = split_in_half L in
            let L1 = fst IntList IntList twolists in
            let L2 = snd IntList IntList twolists in
            merge (mergesort L1) (mergesort L2))
        unit
;;

let L2 = cons 1 null;;
(is_sorted L2);;
/*(length L2);;
split_in_half L2;;*/


let L1 = cons 1 (cons 2 (cons 3 (cons 4 (cons 5 null))));;
(length L1);;
(is_sorted L1);;
let twolists = split_in_half L1;; 
print_list (fst IntList IntList twolists);;
print_list (snd IntList IntList twolists);;

let L3 = cons 3 (cons 2 (cons 9 (cons 4 (cons 1 null))));;
let sorted_L3 = mergesort L3;;
is_sorted L3;;
is_sorted sorted_L3;;
(length L3);;
(length sorted_L3);;

print 3;;
print_list L3;;

print_list (merge (cons 1 (cons 3 (cons 5 null)))
           (cons 2 (cons 4 (cons 6 null))));;

print_list sorted_L3;;

