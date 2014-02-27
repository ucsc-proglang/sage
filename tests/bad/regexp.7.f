let Char :* = Int;; /* unicode */

datatype String =
  Null of (u:Unit)
| Cons of (x:Char) (r:String)
;;

datatype Regexp =
  AnyChar of (u:Unit)
| Alphanum of (u:Unit)
| Kleene of (r:Regexp)
| Concat of (x:Regexp) (y:Regexp)
| Or of (x:Regexp) (y:Regexp)
| Empty of (u:Unit)
;;

let isAlphaNum (c:Char) = (and (65 <= c) (c <= 92));;

let rec isEmpty (x:Regexp)  : Bool =
  caseRegexp x Bool
    /* AnyChar */  (fn u => false)
    /* Alphanum */ (fn u => false)
    /* Kleene */   (fn r => false)
    /* Concat */   (fn x y => false)
    /* Or */       (fn x y => false)
    /* Empty */    (fn u => true)
;;

let rec match_regexp (x:Regexp) (k:Regexp) (s:String) : Bool =
  (caseRegexp x Bool
    /* AnyChar */
    (fn u => 
     caseString s Bool
       /* Null */
       (fn u => false)
       (fn a d => match_regexp (Empty unit) k d))
    /* Alphanum */
    (fn u => 
     caseString s Bool
       /* Null */
       (fn u => false)
       (fn a d => 
        if[Bool] (isAlphaNum a)
        then match_regexp (Empty unit) k d
        else false))
    /* Kleene */
    (fn r => match_regexp (Or (Empty unit) (Concat r (Kleene r))) k s)
    /* Concat */
    (fn x y => match_regexp x (Concat y k) x) /* BUG: replaced s by x */
    /* Or */ 
    (fn x y => 
     if[Bool] (match_regexp x k s)
     then true
     else match_regexp y k s)
    /* Empty */
    (fn u => 
     if[Bool] (isEmpty k)
     then (caseString s Bool
             (fn u => true)
             (fn a d => false))
     else match_regexp k (Empty unit) s));;

let readString (u:Unit) : String = Cons 4 (Null unit);; /* not alphanum */

let executeSQLquery(query:String) : Int = /* ... */ 0;;

/********** Client Code **********************/

let valid_name_regexp = Kleene (Alphanum unit);;
let isValidName (s:String) : Bool = 
  match_regexp valid_name_regexp (Empty unit) s;;
let Name = (x:String.isValidName x);;


/* Following specification foils attacks such as:
Attacks:
     user gives pwd: foo or true
     user gives name: admin -- (-- starts comment)
*/

let authenticate(username:Name) (password:Name) : Bool =
  let query = Null unit in
  /* "SELECT count(*) FROM client WHERE name ="^username^" and pwd="^password */
  executeSQLquery(query) >0
;; 


/*
let fixed_authenticate (username:String) (password:String) =
  if[Bool] (and (isValidName username) (isValidName password)) 
  then authenticate username password
  else false
;; 
*/

let fixed_authenticate (username:String) (password:String) =
  if[Bool] (isValidName username)
  then 
    (if[Bool] (isValidName password) 
    then authenticate username password
    else false)
  else false
;; 

let username = readString unit in
let password = readString unit in
fixed_authenticate username password;;



/*
authenticate (readString unit) (readString unit);;
*/ 
