/*SPECS: http://pauillac.inria.fr/ocaml/htmlman/libref/List.html*/


datatype Pair (X:*) (Y:*) = 
      Pcons of (x:X) (y:Y);;

datatype List (T:*) = 
  Null of (u:Unit)
| Cons of (x:T) (r:(List T));;

let isEmpty (T:*) (l:(List T)):Bool =
      caseList T l Bool
      (fn u => true)
      (fn x r => false);;

let nonemptyList (T:*):*= (x:(List T).(not (isEmpty T x)));;

let fail (T:*) :T = (cast Int T (cast Bool Int false));;

let rec length (T:*) (l:List T):Int = 
  (caseList T l Int
     (fn u => 0)
     (fn x r => 1 + (length T r)));;

let hd (T:*) (l:(List T)) : T =
      caseList T l T
      (fn u => (fail T)) 
      (fn x r => x);;

let tl (T:*) (l:(List T)) : (List T) =
      caseList T l (List T)
      (fn u => (fail (List T)))
      (fn x r => r);;

let rec nth (T:*) (l:(List T)) (n:Int): T =
      caseList T l T
      (fn u => (fail T))
      (fn x r => 
         (if [T] (n = 0) 
         then x
         else (nth T r (n-1))));;

let rec revhelper (T:*) (l:(List T)) (ret:(List T)) : (List T) =
      caseList T l (List T)
      (fn u => ret)
      (fn x r => (revhelper T r (Cons T x ret)));;

let rec rev (T:*) (l:(List T)) : (List T) =
      revhelper T l (Null T unit);;

let rec append (T:*) (l1:(List T)) (l2:(List T)) : (List T) = 
      caseList T l1 (List T)
      (fn u => l2)
      (fn x r => (Cons T x (append T r l2)));;
  
let rec rev_append (T:*) (l1:(List T)) (l2:(List T)) : (List T) = 
      caseList T l1 (List T)
      (fn u => l2)
      (fn x r => (append T r (Cons T x l2)));;

let rec concat (T:*) (l:(List (List T))) : (List T) =
      caseList (List T) l (List T)
      (fn u => (Null T unit))
      (fn x r => (append T x (concat T r)));;

let flatten = concat;;

/*-------------ITERATORS--------------------*/
/*Since we have no side effects does iter matter?*/

let rec iter (T:*) (f:T->Unit) (l:(List T)) : Unit =
      caseList T l Unit
      (fn u => unit)
      (fn x r => (fn (u:Unit) => (iter T f r)) (f x));;

let rec map (S:*) (T:*) (f:(S->T)) (l:(List S)) :(List T) =
      caseList S l (List T)
      (fn u => (Null T unit))
      (fn x r => (Cons T (f x) (map S T f r)));;

let rec rev_map_helper (S:*) (T:*) (f:(S->T)) (l:(List S)) (ret:List T):
      (List T) =
      caseList S l (List T)
      (fn u => ret)
      (fn x r => rev_map_helper S T f r (Cons T (f x) ret));;

let rev_map (S:*) (T:*) (f:(S->T)) (l:(List S)) : (List T) =
      rev_map_helper S T f l (Null T unit);;

let rec fold_left (S:*) (T:*) (f:(T->S->T)) (base:T) (l:(List S)): T =
      caseList S l T
      (fn u => base)
      (fn x r => (f (fold_left S T f base r) x));;

let rec fold_right (S:*) (T:*) (f:(S->T->T)) (base:T) (l:(List S)): T =
      caseList S l T
      (fn u => base)
      (fn x r => (f x (fold_right S T f base r)));;

/*-------------ITERATORS ON TWO LISTS-----------------*/

let rec iter2 (S:*) (T:*) (f:(S->(T->Unit))) (l1:(List S)) (l2:(List T)) 
      : Unit = 
      caseList S l1 Unit
      (fn u1 =>
          (caseList T l2 Unit
          (fn u => unit)
          (fn x r => (fail Unit))))
      (fn x1 r1 =>
          (caseList T l2 Unit
          (fn u2 => (fail Unit))
          (fn x2 r2 => ((fn (u:Unit) => (iter2 S T f r1 r2)) (f x1 x2)))));;

let rec map2 (S:*) (T:*) (Z:*) (f:(S->(T->Z))) (l1:(List S)) (l2:(List T)):
      (List Z)=
      caseList S l1 (List Z)
      (fn u1 => 
          (caseList T l2 (List Z)
          (fn u => (Null Z unit))
          (fn x r => (fail (List Z)))))
      (fn x1 r1 => 
          (caseList T l2 (List Z)
          (fn u2 => (fail (List Z)))
          (fn x2 r2 => (Cons Z (f x1 x2) (map2 S T Z f r1 r2)))));;

let rec rev_map_helper2 (S:*) (T:*) (Z:*) (f:(S->(T->Z))) (l1:(List S)) 
      (l2:(List T)) (ret:(List Z)) : (List Z) =
      caseList S l1 (List Z)
      (fn u1 => 
          (caseList T l2 (List Z)
          (fn u => ret)
          (fn x r => (fail (List Z)))))
      (fn x1 r1 => 
          (caseList T l2 (List Z)
          (fn u2 => (fail (List Z)))
          (fn x2 r2 => (rev_map_helper2 S T Z f r1 r2 (Cons Z (f x1 x2) ret)))));;

let rev_map2 (S:*) (T:*) (Z:*) (f:(S->(T->Z))) (l1:(List S)) (l2:(List T)) 
      : (List Z) =
      rev_map_helper2 S T Z f l1 l2 (Null Z unit);;

let rec fold_left2 (S:*) (T:*) (Z:*) (f:(Z->(S->(T->Z)))) (base:Z) 
      (l1:(List S)) (l2:(List T)): Z =
      caseList S l1 Z
      (fn u1 => 
          (caseList T l2 Z 
          (fn u => base)
          (fn x r => (fail Z))))
      (fn x1 r1 => 
          (caseList T l2 Z
          (fn u2 => (fail Z))
          (fn x2 r2 => (f (fold_left2 S T Z f base r1 r2) x1 x2))));; 

let rec fold_right2 (S:*) (T:*) (Z:*) (f:(S->(T->(Z->Z)))) (base:Z) 
      (l1:(List S)) (l2:(List T)): Z =
      caseList S l1 Z
      (fn u1 => 
          (caseList T l2 Z
          (fn u => base)
          (fn x r => (fail Z))))
      (fn x1 r1 => 
          (caseList T l2 Z
          (fn u2 => (fail Z))
          (fn x2 r2 => (f x1 x2 (fold_right2 S T Z f base r1 r2)))));;

/*-----------LIST SCANNING --------------------*/

let for_all (T:*) (pred:(T->Bool)) (l:(List T)) : Bool =
      fold_left T Bool (fn (res:Bool) (x:T) => (and res (pred x))) true l;; 

let exists (T:*) (pred:(T->Bool)) (l:(List T)) : Bool =
      fold_left T Bool (fn (res:Bool) (x:T) => (or res (pred x))) false l;;

let for_all2 (S:*) (T:*) (pred:(S->(T->Bool))) (l1:(List S)) (l2:(List T)) 
      : Bool =
      fold_left2 S T Bool (fn (res:Bool) (x:S) (y:T)=> (and res (pred x y))) 
          true l1 l2;; 

let exists2 (S:*) (T:*) (pred:(S->(T->Bool))) (l1:(List S)) (l2:(List T)) 
      : Bool =
      fold_left2 S T Bool (fn (res:Bool) (x:S) (y:T) => (or res (pred x y))) 
          false l1 l2;; 

let mem (T:*) (x:T) (l:(List T)) : Bool =
      exists T (fn (y:T) => (eq y x)) l;;

/*MEMQ does not exist for us because only have one kind of equality*/


/*------------------- List searching ---------------*/

let rec find (T:*) (pred:(T->Bool)) (l:(List T)) : T =
      caseList T l T
      (fn u => (fail T))
      (fn x r => (if [T] (pred x)
                 then x
                 else (find T pred r)));;
      
let rec filter (T:*) (pred:(T->Bool)) (l:(List T)) : (List T) =
      caseList T l (List T)
      (fn u => (Null T unit))
      (fn x r =>  (if [List T] (pred x)
                 then (Cons T x (filter T pred r))
                 else (filter T pred r)));;

let findall = filter;;

let rec partition (T:*) (pred:(T->Bool)) (l:(List T)) : 
      (Pair (List T) (List T)) =
      caseList T l (Pair (List T) (List T))
      (fn u => (Pcons (List T) (List T) (Null T unit) (Null T unit)))
      (fn x r => 
          (casePair (List T) (List T) (partition T pred r) 
          (Pair (List T) (List T))
          (fn l1 l2 =>
                (if [(Pair (List T) (List T))] (pred x)
                 then (Pcons (List T) (List T) (Cons T x l1) l2)
                 else (Pcons (List T) (List T) l1 (Cons T x l2))))));;

/*-----------Association Lists-----------------*/

let assoc (S:*) (T:*) (key:S) (al:(List (Pair S T))) : T =
      let aPair = (find (Pair S T) 
                  (fn (x:(Pair S T)) => 
                    (casePair S T x Bool
                      (fn s t => (eq key s))))
                  al) in
      (casePair S T aPair T (fn sa ta => ta));;

/*assq is meaningless because only structural equality exists*/

let mem_assoc (S:*) (T:*) (key:S) (al:(List (Pair S T))) : Bool =
      exists (Pair S T) 
           (fn (x:(Pair S T)) => (casePair S T x Bool (fn s t => (eq key s))))
            al;;

/*mem_assq is meaningless because only structural equality exists*/

let remove_assoc (S:*) (T:*) (key:S) (al:(List (Pair S T))) : 
      (List (Pair S T)) =
      filter (Pair S T)
         (fn (x:(Pair S T)) => 
                    (casePair S T x Bool
                      (fn s t => (not (eq key s)))))
         al;;

/*remove_assq is meaningless because only structural equality exists*/

/*-----------------List of Pairs ------------------------*/

let rec split (S:*) (T:*) (pl:(List (Pair S T))) : (Pair (List S) (List T)) =
      caseList (Pair S T) pl (Pair (List S) (List T))
      (fn u => (Pcons (List S) (List T) (Null S unit) (Null T unit)))
      (fn x r => 
          (casePair (List S) (List T) (split S T r) (Pair (List S) (List T))
          (fn ls lt => (casePair S T x (Pair (List S) (List T))
            (fn s t => (Pcons (List S) (List T) (Cons S s ls) (Cons T t lt)))))));; 

let rec combine (S:*) (T:*) (l1:(List S)) (l2:(List T)) : (List (Pair S T)) =
      caseList S l1 (List (Pair S T))
      (fn u1 => 
          (caseList T l2 (List (Pair S T))
          (fn u => (Null (Pair S T) unit))
          (fn x r => (fail (List (Pair S T))))))
      (fn x1 r1 =>
          (caseList T l2 (List (Pair S T))
          (fn u2 => (fail (List (Pair S T))))
          (fn x2 r2 => (Cons (Pair S T) (Pcons S T x1 x2) (combine S T r1 r2)))));; 

/*EXAMPLES*/
let sum = fold_right Int Int add 0;;

let p = Cons Int 3 (Null Int unit);;

(length Int p);;

sum p;;

let p2 = Cons Int 5 p;;

length Int p2;;
sum p2;;

isEmpty Int p;;

hd Int p2;;
tl Int p;;
/*hd Int (tl Int p);; should fail*/
print (nth Int p2 0);;
print (nth Int p2 1);;
/*nth Int p2 2;; should fail*/
let p3 = rev Int p2;;
print (nth Int p3 0);;
print (nth Int p3 1);;
let p4 = append Int p2 p3;;
print (nth Int p4 0);;
print (nth Int p4 1);;
print (nth Int p4 2);;
print (nth Int p4 3);;
let p5 = rev_append Int p3 p2;;
print (nth Int p5 0);;
print (nth Int p5 1);;
print (nth Int p5 2);;
print (nth Int p5 3);;
/* BUG: (Int) should be (List Int) */
let p6_pre = (Cons (List Int) p2 (Cons (List Int) p3 (Null (Int) unit)));;
let p6 = flatten Int p6_pre;;
print (nth Int p6 0);;
print (nth Int p6 1);;
print (nth Int p6 2);;
print (nth Int p6 3);;

