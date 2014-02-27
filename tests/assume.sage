let eq3 = x:Int.x = 3;;
let g1 = x:Int.x >= 1;;
let g0 = x:Int.x >= 0;;

assume :- eq3 <: g1;;
assume :- g1 <: g0;;

assumenot :- eq3 <: x:Int.x >= 4;;

(3 as g1) as g0;;

assume x:(x:Int. x>=1), u:(u:Unit.(not (leq x 2))) 
          :- (z:Int. z=x-1) <: (x:Int. x>=1);;

