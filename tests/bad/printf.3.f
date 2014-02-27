let Char :* = Int;;
let GTInt (lo:Int) :* = (x:Int.(x>lo));;
let GTEInt (lo:Int) :* = (x:Int.(lo<=x));;
let LTInt (hi:Int) :* = (x:Int.(x<hi));; 
let RInt (lo:Int) (hi:Int) :* = (x:Int. (and (lo<=x)  (x<hi)));;

datatype String =
  SNull of (u:Unit)
| SCons of (x:Char) (r:String)
;;
   
   let rec concat (s1:String) (s2:String) : String=
     caseString s1 String
       (fn u => s2)
       (fn x r => (SCons x (concat r s2)));;

let chomp (s:String) : String =
      caseString s String
      (fn u => s)
      (fn c r => r);;  

let stringify (c:Char) :String = (SCons c (SNull unit));;

let chareq (x:Char) (y:Char) :Bool = inteq x y;;

let rec stringeq (s1:String) (s2:String) :Bool = 
  caseString s1 Bool
    (fn u1 => 
     (caseString s2 Bool
        (fn u => true)
        (fn c r => false)))
    (fn c1 r1 => 
     (caseString s2 Bool
        (fn u2 => false)
        (fn c2 r2 => 
         (if [Bool] (chareq c1 c2)
         then (stringeq r1 r2)
         else false))))
;;

/*--------------Constant Strings and Chars--------------------------*/

let charPercent = 37 as Char;; 
    
let charZero = 48 as Char;;
let charOne = 49 as Char;;
let charTwo = 50 as Char;;
let charThree = 51 as Char;;
let charFour = 52 as Char;;
let charFive = 53 as Char;;
let charSix = 54 as Char;;
let charSeven = 55 as Char;;
let charEight = 56 as Char;;
let charNine = 57 as Char;;

let charA = 65 as Char;; /*unicode decimal number for 'A'*/
let charB = 66 as Char;; /*unicode decimal number for 'B'*/
let charD = 68 as Char;; /*unicode decimal number for 'D'*/
let charE = 69 as Char;; /*unicode decimal number for 'E'*/
let charF = 70 as Char;; /*unicode decimal number for 'F'*/
let charL = 76 as Char;; /*unicode decimal number for 'L'*/
let charR = 82 as Char;; /*unicode decimal number for 'R'*/
let charS = 83 as Char;; /*unicode decimal number for 'S'*/
let charT = 84 as Char;; /*unicode decimal number for 'T'*/
let charU = 85 as Char;; /*unicode decimal number for 'U'*/
let charDash = 45 as Char;; /*unicode decimal number for '-'*/

let strPerD = (SCons charPercent (SCons charD (SNull unit)));;
let strPerB = (SCons charPercent (SCons charB (SNull unit)));;
let strPerS = (SCons charPercent (SCons charS (SNull unit)));;
/*
let stringTrue = SCons charT (SCons charR (SCons charU (SCons charE 
      (SNull unit))));;

let stringFalse =  SCons charF  (SCons charA  (SCons charL  
      (SCons charS  (SCons charE (SNull unit)))));;

/*-------------------INT->STRING---------------------------*/
         
let numeral_to_char (i:(RInt 0 10)) : Char =
      if [Char] (inteq i 0)
      then charZero
      else (if [Char] (inteq i 1)
         then charOne
         else (if [Char] (inteq i 2)
            then charTwo
            else (if [Char] (inteq i 3)
               then charThree
               else (if [Char] (inteq i 4)
                  then charFour
                  else (if [Char] (inteq i 5)
                     then charFive
                     else (if [Char] (inteq i 6)
                        then charSix
                        else (if [Char] (inteq i 7)
                           then charSeven
                           else (if [Char] (inteq i 8)
                              then charEight
                              else charNine
                                 ))))))));;

let rec mod (i:(GTEInt 0)) (d:(GTInt 0)) : (RInt 0 d) =
      if [RInt 0 d] (i < d)
      then i
      else (mod (i-d) d);;

let rec fd_helper (i:(GTEInt 0)) (d:(GTInt 0)) (res:(GTEInt 0)) : (GTEInt 0) =
      if [GTEInt 0] (i < d)
      then res
      else (fd_helper (i-d) d (res+1));;

let floor_divide (i:(GTEInt 0)) (d:(GTInt 0)): (GTEInt 0) =
      fd_helper i d 0;;

let rec helper_int_to_string (i:(GTEInt 0)) (s:String) : String = 
      if [String] (inteq i 0)
      then s
      else (let new_s = (SCons (numeral_to_char (mod i 10)) s) in
             (helper_int_to_string (floor_divide i 10) new_s));;

let int_to_string (i:Int) (ret:String) : String = 
      if [String] (inteq i 0) 
      then (stringify charZero)
      else (if [String] (i < 0) 
         then (concat 
           (SCons charDash (helper_int_to_string (0 - i) (SNull unit)))
           ret)
         else (concat (helper_int_to_string i (SNull unit)) ret));;

/*------------BOOL->STRING -------------*/

let bool_to_string (b:Bool) (ret:String) : String =
      if [String] b
      then (concat stringTrue ret)
      else (concat stringFalse ret)
   ;;
*/


/*------------THE TYPE --------------*/

let rec Data (fmt:String) : * =
  (caseString fmt *
    (fn u => Unit)
    (fn x1 r1 => 
     if[*] (chareq x1 charPercent) 
     then 
       (caseString r1 *
         (fn u => Unit)
         (fn x2 r2 =>
          if[*] (chareq x2 charD)
          then (Int -> (Data r2))
          else if[*] (chareq x2 charB)
          then (Bool -> (Data r2))
          else Data r2))
     else Data r1))
;;


/*---------PRINTF----------------*/

let rec printf (fmt:String) : Data fmt =
  let t = (Data fmt) in
  (caseString fmt (Data fmt)
        (fn u => unit)
        (fn x1 r1 => 
         if[Data fmt] (chareq x1 charPercent) 
         then 
           (caseString r1 (Data fmt)
             (fn u => unit)
             (fn x2 r2 =>
              if[Data fmt] (chareq x2 charD)
              then 
                (fn (i:Bool) =>  /* BUG: Int -> Bool */
                  let dummy = print i in 
                  printf r2)
              else if[Data fmt] (chareq x2 charB)
              then 
                (fn (b:Bool) => 
                  let dummy = print b in 
                  printf r2)
              else 
                (let d = print x1 in 
                let d = print x2 in
                printf r2)))
         else 
           let d = print x1 in
           printf r1))
;;



/*---------PRINTF----------------

let rec printf (fmt:String) =
  let t = Dynamic in
  (caseString fmt t
        (fn u => unit)
        (fn x1 r1 => 
         if[t] (chareq x1 charPercent) 
         then 
           (caseString r1 t
             (fn u => unit)
             (fn x2 r2 =>
              if[t] (chareq x2 charD)
              then 
                (fn (i:Int) => 
                  let dummy = print i in 
                  printf r2)
              else if[t] (chareq x2 charB)
              then 
                (fn (b:Bool) => 
                  let dummy = print b in 
                  printf r2)
              else 
                (let d = print x1 in 
                let d = print x2 in
                printf r2)))
         else 
           let d = print x1 in
           printf r1))
;;

*/

printf (SCons charPercent (SCons charB (SCons charPercent (SCons charB (SNull unit)))));;


