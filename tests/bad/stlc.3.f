/*WELLTYPED*/

let Var = Int;;

datatype Type =
  UnitTy of (u:Unit)
| Arrow of (dom:Type) (range:Type)
| Failure of (u:Unit)
/* Not sure what to do about exceptions... */
;;

let unitty = UnitTy unit;;
let failure = Failure unit;;

datatype Term =
  UnitTm of (u:Unit)
| Variable of (x:Var)
| Lambda of (ty:Type) (body:Term)
| App of (func:Term) (arg:Term)
;;

let unittm = UnitTm unit;;

datatype Context =
  EmptyCtx of (u:Unit)
| Extend of (ty:Type) (ctx:Context)
;;

let emptyctx = EmptyCtx unit;;

let rec print_ty (ty:Type) : Unit =
  caseType ty Unit
    (fn u => print "Unit")
    (fn dom range => 
       print "("; print_ty dom; print " -> "; print_ty range; print ")")
    (fn u => print "FAILURE")
;;

let rec print_tm (tm:Term) : Unit =
  caseTerm tm Unit
    (fn u => print "unit")
    (fn x => print x)
    (fn ty body => 
      print "(fun:"; print_ty ty; print "."; print_tm body; print ")")
    (fn f arg =>
      print "("; print_tm f; print " "; print_tm arg; print ")")
;;


let get_type (ctx:Context) (x:Var) : Type =
  let rec get_type_rec (ctx:Context) (count:Var) (x:Var) : Type =
    caseContext ctx Type
      (fn u => failure)
      (fn ty ctx_tail =>
        if [Type] count = x then ty else get_type_rec ctx_tail (count+1) x)
  in
  get_type_rec ctx 0 x
;;

let rec typeeq (ty1:Type) (ty2:Type) : Bool =
  caseType ty1 Bool
    (fn u => caseType ty2 Bool
               (fn u => true)
               (fn dom range => false)
               (fn u => false))
    (fn dom range => caseType ty2 Bool
               (fn u => false)
               (fn dom2 range2 => (typeeq dom dom2) && (typeeq range range2))
               (fn u => false))
    (fn u => false)
;;

let rec termeq (tm1:Term) (tm2:Term) : Bool =
  caseTerm tm1 Bool
     (fn u => caseTerm tm2 Bool
                (fn u => true)
                (fn x2 => false)
                (fn ty2 body2 => false)
                (fn f2 arg2 => false))
     (fn x1 => caseTerm tm2 Bool
                (fn u => false)
                (fn x2 => x1 = x2)
                (fn ty2 body2 => false)
                (fn f2 arg2 => false))
     (fn ty1 body1 => caseTerm tm2 Bool
                (fn u => false)
                (fn x2 => false)
                (fn ty2 body2 => (typeeq ty1 ty2) && (termeq body1 body2))
                (fn f2 arg2 => false))
     (fn f1 arg1 => caseTerm tm2 Bool
                (fn u => false)
                (fn x2 => false)
                (fn ty2 body2 => false)
                (fn f2 arg2 => (termeq f1 f2) && (termeq arg1 arg2)))
;;

let rec typeof (ctx:Context) (t:Term) : Type =
  caseTerm t Type
     (fn u => unitty)
     (fn x => get_type ctx x)
     (fn ty body => Arrow ty (typeof (Extend ty ctx) body))
     (fn func arg =>
       caseType (typeof ctx func) Type
         (fn u => Failure unit)
         (fn dom range =>
            let argty = typeof ctx arg in
            if [Type] typeeq argty dom 
            then range
            else (Failure unit))
         (fn u => Failure unit))
;;

let is_well_typed (ctx:Context) (tm:Term) : Bool =
	caseType (typeof ctx tm) Bool
		(fn u => true)
		(fn dom range => true)
		(fn u => false)
;;


let rec isval (tm:Term) : Bool =
  caseTerm tm Bool
    (fn u => true)
    (fn x => true)
    (fn ty body => true)
    (fn func arg => false)
;;

let rec shift_above (threshold:Int) (distance:Int) (tm:Term) : Term =
  /* print "shift_above ";  
  print threshold; print " "; print distance; newline unit; */
  caseTerm tm Term
    (fn u => tm)
    (fn x => if [Term] x > threshold then Variable (x+distance) else tm)
	/* BUG: x > threshold should be x >= threshold */
    (fn ty body => Lambda ty (shift_above (threshold+1) distance body))
    (fn f arg => App (shift_above threshold distance f)
                     (shift_above threshold distance arg))
;;

let same_type_term (tm:Term) : * =
	(t : Term .
		if is_well_typed emptyctx tm then
			(typeeq (typeof emptyctx tm) (typeof emptyctx t))
		else
			true)
;;

let shift_tm (distance:Int) (tm:Term) : Term =
  /* print "shift_tm"; newline unit; */
  shift_above 0 distance tm;;

let rec subst_tm (x:Var) (into:Term) (inserted:Term) : Term =
  /* print "subst_tm"; newline unit; */
  caseTerm into Term
    (fn u => into)
    (fn y => if [Term] x = y then inserted else into)
    (fn ty body => Lambda ty (subst_tm (x+1) body (shift_tm 1 inserted)))
    (fn f arg => App (subst_tm x f inserted) (subst_tm x arg inserted))
;;

/*	Refining this return value created another Maybe without
	removing the one in eval1. NOTE: This function is
	ill-typed; the type Term is not sufficiently refined.  */
let subst_top_tm (body:Term) (arg:Term) : Term = /* (same_type_term body) = */
  /* print "subst_top_tm"; newline unit; */
  shift_tm (0 - 1) (subst_tm 0 body (shift_tm 1 arg))
;;

let rec eval1 (tm:Term/*.is_well_typed emptyctx tm*/) : (same_type_term tm) =
  let return_type =  same_type_term tm in
  /* print_tm tm; newline unit; */
  caseTerm tm return_type
     (fn u => tm)
     (fn x => tm)
     (fn ty body => tm)
     (fn func arg =>
       caseTerm func return_type
         (fn u => tm)
         (fn x => tm)
         (fn ty body =>
           if [return_type] isval arg then
             subst_top_tm body arg
           else 
             App func (eval1 arg))
         (fn fn1 fn2 => App (eval1 func) arg))
;;

let rec eval_nf (tm:Term/*.is_well_typed emptyctx tm*/) : (same_type_term tm) =
	let tm2 = eval1 tm in
	if [same_type_term tm] (termeq tm tm2) then
		tm
	else (
		/*print_tm tm2; newline unit; */
		eval_nf tm2)
;;

/*
let identity = (Lambda unitty (Variable 0));;
let tm1 = (App identity unittm);;

print_tm tm1; newline unit;
print_tm (eval1 tm1); newline unit;;
print_tm (eval_nf tm1); newline unit;;
print_tm (eval_nf (App identity (App identity unittm))); newline unit;;


let church_numeral = Arrow (Arrow unitty unitty) (Arrow unitty unitty);;

let one =	(Lambda (Arrow unitty unitty) 
				(Lambda unitty
					(App (Variable 1) (Variable 0))));;

let add =	(Lambda church_numeral
			(Lambda church_numeral
			(Lambda (Arrow unitty unitty)
			(Lambda unitty
				(App 
					(App (Variable 3) (Variable 1))
						(App (App (Variable 2) (Variable 1)) (Variable 0))))
		)));;


print_tm one; newline unit;
print_ty (typeof emptyctx one); newline unit;;
print_ty (typeof emptyctx (App one identity)); newline unit;;
print_tm (App one identity); newline unit;;
print_tm (eval1 (App one identity)); newline unit;;
print_tm (eval_nf (App (App one identity) unittm)) ;;
print_tm (eval_nf (App (App add one) one));;

*/

