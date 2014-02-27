(** -----------------------SFORMULA MODULE ----------------------- 
The sformula module preprocesses the proving juedgement E|- s 
to make the Sage Syntax palatable to Simplify. At the end of the 
prove method call_simplify is called which is located in the simplify
module.
*)

open Syntax
open Support.Pervasive

(** 
prove - the method accepts the judgement E|- s, converts it into legal
Simplify formulas and then sends it to Simplify.call_simplify. 
The returned value is the judgement Simplify has made: True/Maybe/False 
NOTE: If it returns [False], it will always be [False None] 
*)

val prove: context -> term -> 'a option bool3

