(* fun 1 *)
fun alternate (xs:int list) = 
  let fun helper (xs:int list, flag: bool) =
      if null xs
      then 0 
      else if flag
      then hd xs + helper(tl xs, not flag)
      else ~(hd xs) + helper(tl xs, not flag)
  in
      helper(xs, true)
  end
      
(* fun 2 *)
fun min_max (xs:int list) =
  if null (tl xs)
  then (hd xs, hd xs)
  else
      let val mm = min_max(tl xs)
	  val tl_min = #1 mm
	  val tl_max = #2 mm
      in
	  if hd xs > tl_max
	  then (tl_min, hd xs)
	  else if hd xs < tl_min
	  then (hd xs, tl_max)
	  else mm
      end
	  
(* fun 3 *)
fun cumsum (xs:int list) =
  let fun accum (xs:int list, acc:int) =
	if null xs
	then []
	else (hd xs + acc)::accum(tl xs, acc + hd xs)
  in
      accum(xs, 0)
  end
      
(* fun 4 *)
fun greeting (ostr:string option) =
  if isSome ostr
  then "Hello there, " ^ valOf ostr
  else "Hello there, you"

(* fun 5 *)
fun number_repeat (x:int, y:int) =
  if y = 0
  then []
  else x::number_repeat(x, y-1)
			  
fun repeat (ts:int list * int list) =
  let val lst1 = #1 ts
      val lst2 = #2 ts
  in
      if null lst1 orelse null lst2
      then []
      else number_repeat(hd lst1, hd lst2) @ repeat(tl lst1, tl lst2)
  end

(* fun 7 *)
fun addOpt (xo:int option, yo:int option) =
  if isSome xo andalso isSome yo
  then SOME(valOf xo + valOf yo)
  else NONE
	   
(* fun 8 *)
fun addAllOpt (xolst:int option list) =
  if null xolst
  then NONE
  else if isSome (hd xolst)
  then
      let
	  val hdval = valOf (hd xolst)
	  val tlval = addAllOpt(tl xolst)
      in
	  if isSome tlval
	  then SOME(hdval + valOf tlval)
	  else SOME(hdval)
      end
  else addAllOpt(tl xolst)


fun all (xs:bool list) =
  if null xs
  then true
  else if (hd xs) = false
  then false
  else all(tl xs)

fun zip (xs:int list, ys:int list) =
  if null xs orelse null ys
  then []
  else (hd xs, hd ys)::zip(tl xs, tl ys)

fun list_len (xs:int list) =
  if null xs
  then 0
  else 1 + list_len(tl xs)
			  
fun zipRecycle (xs:int list, ys:int list) =
  let fun helpzip (xs1:int list, xs2:int list, ys:int list, flag:bool) =
	if null ys
	then []
	else if null xs2
	then helpzip(xs1, xs1, ys, flag)
	else
	    if flag
	    then (hd xs2, hd ys)::helpzip(xs1, tl xs2, tl ys, flag)
	    else (hd ys, hd xs2)::helpzip(xs1, tl xs2, tl ys, flag)
  in
      if list_len xs > list_len ys
      then helpzip(ys,ys,xs,false)
      else helpzip(xs,xs,ys,true)
  end

fun divide (xs:int list) =
  let fun helpdiv (xs:int list, first:bool) =
	if null xs
	then ([],[])
	else
	    let val tlres = helpdiv(tl xs, not first)
		val fi = #1 tlres
		val se = #2 tlres
	    in
		if first
		then (hd xs::fi, se)
		else (fi, hd xs::se)
	    end
  in
      helpdiv(xs, true)
  end


fun fullDivide (k:int, n:int) =
  if n mod k <> 0
  then (0, n)
  else
      let val revres = fullDivide(k, n div k)
	  val k2 = #1 revres
	  val n2 = #2 revres
      in
	  (1+k2, n2)
      end

fun isPrime (n:int) =
  let fun helper (i:int, n:int) =
	if n=i
	then true
	else if n mod i = 0
	then false
	else helper(i+1, n)
  in
      if n < 2
      then false
      else helper(2, n)
  end 

fun factorize (n:int) =
  let fun helper (i:int, n:int) =
	if i = n
	then []
	else if isPrime(i)
	then
	    let val r = fullDivide(i, n)
		val c = #1 r
	    in
		if c=0
		then helper(i+1, n)
		else (i,c)::helper(i+1,n)
	    end
	else helper(i+1, n)
  in
      helper(2, n)
  end

fun power ((m,n): (int*int)) =
  if n <= 0
  then 1
  else m * power(m, n-1)

fun multiply (xs:(int*int) list) =
  if null xs
  then 1
  else let val tup = hd xs
	   val a = #1 tup
	   val b = #2 tup
       in
	   power(a,b)*multiply(tl xs)
       end
	   
fun splitAt (xs:int list, p:int) =
  if null xs
  then ([],[])
  else
      let val x = hd xs
	  val res = splitAt(tl xs, p)
	  val lst1 = #1 res
	  val lst2 = #2 res
      in
	  if x < p
	  then (x::lst1, lst2)
	  else (lst1, x::lst2)
      end

fun sortedMerge (xs:int list, ys:int list) =
  if null xs
  then ys
  else if hd xs < hd ys
  then (hd xs)::sortedMerge(tl xs, ys)
  else (hd ys)::sortedMerge(xs, tl ys)

fun qsort (xs:int list) =
  if null xs
  then []
  else
      let val pivot = hd xs
	  val list_tup = splitAt(tl xs, pivot)
	  val lst1 = qsort(#1 list_tup)
	  val lst2 = qsort(#2 list_tup)
      in
	  sortedMerge(lst1, pivot::lst2)
      end
	  
(* hard challenge: fun all_products (xs:(int*int) list): int list = ... *)
(* factorize -> unzip -> 1,2,...,n *0	  
