


fn (p:Int->Bool) (x:Int) =>
   if p x
   then x as {y:Int|p y}
   else 0;;


