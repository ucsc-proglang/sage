/*
A red-black tree is a binary search tree which has the following red-black properties:

   1. Every node is either red or black.
   2. Every leaf (NULL) is black.
   3. If a node is red, then both its children are black.
   4. Every simple path from a node to a descendant leaf contains the same number of black nodes. 

   3. implies that on any path from the root to a leaf, red nodes must not be adjacent.
      However, any number of black nodes may appear in a sequence. 

- Note: This implementation is not packaged as a module 
-- as in Okasaki because it requires multiple parameter type classes
-- which are not available in Hugs

data Color = R | B
data RedBlackSet a = E | T Color (RedBlackSet a) a (RedBlackSet a)

balance B (T R (T R a x b) y c) z d = T R (T B a x b) y (T B c z d)
balance B (T R a x (T R b y c)) z d = T R (T B a x b) y (T B c z d)
balance B a x (T R (T R b y c) z d) = T R (T B a x b) y (T B c z d)
balance B a x (T R b y (T R c z d)) = T R (T B a x b) y (T B c z d)
balance color a x b = T color a x b

empty = E

member x E = False
member x (T _ a y b)
   | x < y  = member x a
   | x > y  = member x b
   | True   = True
   
insert x s = T B a y b
  where 
    T _ a y b = ins s
    ins E = T R E x E
    ins s@(T color a y b) 
      | x < y  = balance color (ins a) y b
      | x > y  = balance color a y (ins b)
      | True   = s
    
*/


datatype Tree (mustbeblack:Bool)  =
  Leaf of (u:Unit)
| Node of (i:Int)
      (black:Bool.implies mustbeblack black) 
      (l:Tree (not black)) 
      (r:Tree (not black))
;;

let rec member (n:Int) (mbb:Bool) (t:Tree mbb) : Bool =
  caseTree false t Bool 
    /* Leaf */
    (fn u => false)
    /* Node */
    (fn i b l r =>
     if[Bool] i=n
     then true
     else 
     if[Bool] i<n
     then member n (not b) r
     else member n (not b) l)
;;

/*
balance B (T R (T R a x b) y c) z d = T R (T B a x b) y (T B c z d)
balance B (T R a x (T R b y c)) z d = T R (T B a x b) y (T B c z d)
balance B a x (T R (T R b y c) z d) = T R (T B a x b) y (T B c z d)
balance B a x (T R b y (T R c z d)) = T R (T B a x b) y (T B c z d)
balance color a x b = T color a x b
*/

caseTree;;


let rec balance (mustbeblack:Bool) 
    (i:Int)
    (b:Bool.implies mustbeblack b)  
    (l:Tree (not b) ) 
    (r:Tree (not b) )
    : (Tree mustbeblack )
    = 
  let case5 (u:Unit):Tree mustbeblack = Node mustbeblack i b l r in
  let case4 (u:Unit):Tree mustbeblack = 
    caseTree (not b) r (Tree mustbeblack)
      /* Leaf */
    (fn u => case5 unit)
      /* Node */
    (fn ri rb rl rr =>
     if[Tree mustbeblack] rb
     then case5 unit
     else
       caseTree (not rb) rr (Tree mustbeblack)
         /* Leaf */
       (fn u => case5 unit)
         /* Node */
       (fn rri rrb rrl rrr =>
        if[Tree mustbeblack] rrb
        then case5 unit
        else 
          Node mustbeblack ri false
            (Node true i true l rl)
            (Node true rri true rrl rrr))) in
  case5 unit;;
       

/*
    /* Leaf */
    (fn u => false)
    /* Node */
    (fn i b l r =>)
*/



    




/*
datatype Tree (mustbeblack:Bool) (black_height:Int) =
  Leaf of (u:Unit.black_height=0)
| Node of (i:Int)
      (black:Bool.implies mustbeblack black) 
      (l:Tree (not black) (if[Int] black then black_height-1 else black_height)) 
      (r:Tree (not black) (if[Int] black then black_height-1 else black_height))
;;

let rec member (n:Int) (mbb:Bool) (h:Int) (t:Tree mbb h) : Bool =
  caseTree false h t Bool 
    /* Leaf */
    (fn u => false)
    /* Node */
    (fn i b l r =>
     if[Bool] i=n
     then true
     else 
     if[Bool] i<n
     then member n (not b) (if[Int] b then h-1 else h) r
     else member n (not b) (if[Int] b then h-1 else h) l)
;;

let rec balance (mustbeblack:Bool) (black_height:Int)
    (i:Int)
    (black:Bool.implies mustbeblack black) 
    (l:Tree (not black) (if[Int] black then black_height-1 else black_height)) 
    (r:Tree (not black) (if[Int] black then black_height-1 else black_height))
    : (Tree mustbeblack black_height)
    = Node mustbeblack black_height i black l r;;

*/

/*



let rec insert (n:Int) (t:Tree) : Tree =
  let rec ins (s:Tree) : Tree =
    caseTree s Tree
      (fn u => Node n false (Leaf unit) (Leaf unit) )
      (fn i b l r =>
        if[Tree] n<i
        then balance i b (ins l) r
        else balance i b l (ins r))
  in
  caseTree (ins t) Tree
    (fn u => Leaf unit /* should not happen */)
    (fn i b l r => Node i true l r);;







let rbtree_case (t:RBTree) (Z:*) (f1:Unit->Z) (f2:RBNode->Z) : Z =
  case Unit RBNode Z t f1 f2;;

let empty_tree = inl Unit RBNode unit;;

let is_empty (t:RBTree) =
  rbtree_case t Bool
    (fn (u:Unit) => true)
    (fn (n:RBNode) => false);;

let NERBTree = (t:RBTree.not (is_empty t));;

let mknode (i:Int) (red:Bool) (l:RBTree) (r:RBTree) : RBNode =  
  let tree_pair = pair RBTree RBTree l r in
  let type_pair = pair Bool (Pair RBTree RBTree) red tree_pair in
  let val_pair = pair Int (Pair Bool (Pair RBTree RBTree)) i type_pair in
    val_pair;;

let mktree (i:Int) (red:Bool) (l:RBTree) (r:RBTree) : RBTree =  
    inr Unit RBNode (mknode i red l r);;

let root (t:RBTree) : RBNode =
  rbtree_case t RBNode
    (fn (u:Unit) =>
      pair Int (Pair Bool (Pair RBTree RBTree))
           (0 - 1)
           (pair Bool (Pair RBTree RBTree)
                false
                (pair RBTree RBTree empty_tree empty_tree))
    )
    (fn (n:RBNode) => n);;

let lchild (n:RBNode) : RBTree =
  let p1 = snd Int (Pair Bool (Pair RBTree RBTree)) n in
  let p2 = snd Bool (Pair RBTree RBTree) p1 in
    fst RBTree RBTree p2;;

let rchild (n:RBNode) : RBTree =
  let p1 = snd Int (Pair Bool (Pair RBTree RBTree)) n in
  let p2 = snd Bool (Pair RBTree RBTree) p1 in
    snd RBTree RBTree p2;;

let value (n:RBNode) : Int =
  fst Int (Pair Bool (Pair RBTree RBTree)) n;;

let is_red (n:RBNode) : Bool =
  fst Bool (Pair RBTree RBTree)
      (snd Int (Pair Bool (Pair RBTree RBTree)) n);;

let is_black (n:RBNode) : Bool = not (is_red n);;

let rec black_length (t:RBTree) : Int =
  rbtree_case t Int
    (fn (u:Unit) => 0 /* Should this be 1? */)
    (fn (n:RBNode) =>
      let llength = black_length (lchild n) in
      let rlength = black_length (rchild n) in
      let extra = if[Int] (is_red n) then 0 else 1 in
       if[Int] (llength = rlength) then llength + extra else 0-1);;

let rec valid_arbtree (t:RBTree) : Bool =
  rbtree_case t Bool
    (fn (u:Unit) => true)
    (fn (n:RBNode) =>
      (valid_arbtree (lchild n)) &&
      (valid_arbtree (rchild n)) &&
      (not ((black_length t) = (0 - 1))) &&
      (not ((is_red n) && (is_red (root (lchild n))))) &&
      (not ((is_red n) && (is_red (root (rchild n))))));;
      
let valid_rbtree (t:RBTree) : Bool =
  rbtree_case t Bool
    (fn (u:Unit) => true)
    (fn (n:RBNode) => (not (is_red n)) && (valid_arbtree t));;

let VRBTree = (t:RBTree.valid_rbtree t);;

let color_root_black (t:RBTree) : RBTree =
  rbtree_case t RBTree
    (fn (u:Unit) => t)
    (fn (n:RBNode) => 
      mktree (value n) false (lchild n) (rchild n));;

let has_dd (c1:RBNode->RBTree) (c2:RBNode->RBTree) (t:RBTree) : Bool =
  rbtree_case t Bool
    (fn (u:Unit) => false)
    (fn (n:RBNode) =>
      rbtree_case (c2 n) Bool
        (fn (u:Unit) => false)
        (fn (n:RBNode) => not (is_empty (c1 n))));;

let has_ll (t:RBTree) = has_dd lchild lchild t;;
let has_lr (t:RBTree) = has_dd lchild rchild t;;
let has_rl (t:RBTree) = has_dd rchild lchild t;;
let has_rr (t:RBTree) = has_dd rchild rchild t;;

let r_red (t:RBTree) = is_red (root (rchild (root t)));;
let l_red (t:RBTree) = is_red (root (lchild (root t)));;
let ll_red (t:RBTree) =
  (l_red t) && (is_red (root (lchild (root (lchild (root t))))));;
let lr_red (t:RBTree) = 
  (l_red t) && (is_red (root (rchild (root (lchild (root t))))));;
let rl_red (t:RBTree) = 
  (r_red t) && (is_red (root (lchild (root (rchild (root t))))));;
let rr_red (t:RBTree) =
  (r_red t) && (is_red (root (rchild (root (rchild (root t))))));;

let r_val (t:RBTree) = value (root (rchild (root t)));;
let l_val (t:RBTree) = value (root (lchild (root t)));;
let ll_val (t:RBTree) = value (root (lchild (root (lchild (root t)))));;
let lr_val (t:RBTree) = value (root (rchild (root (lchild (root t)))));;
let rl_val (t:RBTree) = value (root (lchild (root (rchild (root t)))));;
let rr_val (t:RBTree) = value (root (rchild (root (rchild (root t)))));;

let balance (t:RBTree) : RBTree =
  if[RBTree] is_empty t then
    t
  else if[RBTree] has_ll t then
    (let lll = lchild (root (lchild (root (lchild (root t))))) in
    let llr = rchild (root (lchild (root (lchild (root t))))) in
    let lr = rchild (root (lchild (root t))) in
    let r = rchild (root t) in
      if (is_black (root t)) && (l_red t) && (ll_red t) then
        mktree (l_val t) true
          (mktree (ll_val t) false lll llr)
          (mktree (value (root t)) false lr r)
      else
        t)
  else if[RBTree] has_lr t then
    (let ll = (lchild (root (lchild (root t)))) in
    let lrl = lchild (root (rchild (root (lchild (root t))))) in
    let lrr = rchild (root (rchild (root (lchild (root t))))) in
    let r = rchild (root t) in
      if (is_black (root t)) && (l_red t) && (lr_red t) then
        mktree (lr_val t) true
          (mktree (l_val t) false ll lrl)
          (mktree (value (root t)) false lrr r)
      else
        t)
  else if[RBTree] has_rl t then
    (let rll = lchild (root (lchild (root (rchild (root t))))) in
    let rlr = rchild (root (lchild (root (rchild (root t))))) in
    let rr = rchild (root (rchild (root t))) in
    let l = lchild (root t) in
      if (is_black (root t)) && (r_red t) && (rl_red t) then
        mktree (rl_val t) true
          (mktree (value (root t)) false l rll)
          (mktree (r_val t) false rlr rr)
      else
        t)
  else if[RBTree] has_rr t then
    (let rrl = lchild (root (rchild (root (rchild (root t))))) in
    let rrr = rchild (root (rchild (root (rchild (root t))))) in
    let rl = lchild (root (rchild (root t))) in
    let l = lchild (root t) in
      if (is_black (root t)) && (r_red t) && (rr_red t) then
        mktree (r_val t) true
          (mktree (value (root t)) false l rl)
          (mktree (rr_val t) false rrl rrr)
      else
        t)
  else
    t;;
  
let rec insert (i:Int) (t:/*V*/RBTree) : /*V*/RBTree =
  let rec ins (s:RBTree) : RBTree =
    rbtree_case s RBTree
      (fn (u:Unit) => mktree i true empty_tree empty_tree)
      (fn (n:RBNode) =>
        if[RBTree] (i < (value n)) then /* i < n */
            balance (mktree (value n) (is_red n) (ins (lchild n)) (rchild n))
        else
            if[RBTree] (i > (value n)) then /* i > n */
                balance (mktree (value n) (is_red n)
                                (lchild n) (ins (rchild n)))
            else /* i = n */
              s)
  in
    color_root_black (ins t);;
*/
