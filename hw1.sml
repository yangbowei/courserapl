(* fun 1 *)
fun is_older (date1:(int*int*int), date2:(int*int*int)) =
  if #1 date1 < #1 date2
  then true
  else if #1 date1 > #1 date2
  then false
  else
      if #2 date1 < #2 date2
      then true
      else if #2 date1 > #2 date2
      then false
      else
	  if #3 date1 < #3 date2
	  then true
	  else false

(* fun 2 *)
fun number_in_month (dates:(int*int*int) list, month:int) =
  if null dates
  then 0
  else
      if #2 (hd dates) = month
      then 1+number_in_month(tl dates, month)
      else number_in_month(tl dates, month)

(* fun 3 *)
fun number_in_months (dates:(int*int*int) list, months:int list) =
  if null months
  then 0
  else number_in_month(dates, hd months) + number_in_months(dates, tl months)

(* fun 4 *)
fun dates_in_month (dates:(int*int*int) list, month: int) =
  if null dates
  then []
  else
      if #2 (hd dates) = month
      then hd dates::dates_in_month(tl dates, month)
      else dates_in_month(tl dates, month)
      
(* fun 5 *)
fun dates_in_months (dates:(int*int*int) list, months:int list) =
  if null months
  then []
  else dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)
							 
(* fun 6 *)
fun get_nth (str_list:string list, n:int) =
  if n=1
  then hd str_list
  else get_nth(tl str_list, n-1)

(* fun 7 *)
fun date_to_string (date:(int*int*int)) =
  let val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
      fun month_to_string (month:int, month_str: string list) =
	if month=1
	then hd month_str
	else month_to_string(month-1, tl month_str)
  in
      month_to_string(#2 date, months) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
  end
       
(* fun 8 *)
fun number_before_reaching_sum (sum:int, lst:int list) =
  let fun helper (sum:int, lst:int list, acc:int) =
	if sum <= hd lst
	then acc
	else helper(sum - hd lst, tl lst, acc+1)
  in
      helper(sum, lst, 0)
  end

(* fun 9 *)
fun what_month day:int =
  let val month_day = [31,28,31,30,31,30,31,31,30,31,30,31]
  in
      1 + number_before_reaching_sum(day, month_day)
  end

(* fun 10 *)
fun month_range (day1:int, day2:int) =
  let fun helper (day1:int, day2:int, month_list:int list) =
	if day1 > day2
	then month_list
	else helper(day1, day2-1, what_month(day2)::month_list)
  in
      helper(day1, day2, [])
  end

(* fun 11 *)
fun oldest (dates:(int*int*int) list) =
  let
      fun older (date1:(int*int*int), date2:(int*int*int)) =
	if is_older(date1, date2)
	then date1
	else date2
		 
      fun helper (dates:(int*int*int) list, old:(int*int*int)) = 
	if null dates
	then old
	else helper(tl dates, older(old, hd dates))
  in
      if null dates
      then NONE
      else SOME (helper(tl dates, hd dates))
  end

      
(* challenge problem *)
(* helper functions *)
fun exist (num:int, int_list:int list) =
  if null int_list
  then false
  else if num = hd int_list
  then true
  else exist(num, tl int_list)

fun dedup (lst1:int list, lst2: int list) =
  if null lst1
  then lst2
  else if exist(hd lst1, lst2)
  then dedup(tl lst1, lst2)
  else dedup(tl lst1, (hd lst1)::lst2)

(* fun 12 *)
fun number_in_months_challenge (dates:(int*int*int) list, months:int list) =
  number_in_months(dates, dedup(months, []))

(* fun 13 *)
fun dates_in_months_challenge (dates:(int*int*int) list, months:int list) =
  dates_in_months(dates, dedup(months, []))
		 
(* fun 14 *)
fun reasonable_date (date:(int*int*int)) =
  let
      fun isLeapYear(date:(int*int*int)):bool =
	if #1 date mod 400 = 0
	then true
	else if #1 date mod 4 = 0 andalso #1 date mod 100 <> 0
	then true
	else false

      val month_day = [31,28,31,30,31,30,31,31,30,31,30,31]

      fun get_month_day (month:int, month_day:int list) =
	if month = 1
	then hd month_day
	else get_month_day(month-1, tl month_day)
  in
      (* year *)
      if #1 date <= 0
      then false
      (* month *)
      else if #2 date < 1 orelse #2 date > 12
      then false
      (* day *)
      else
	  let
	      val max_day =
		  if isLeapYear(date) andalso #2 date = 2
		  then get_month_day(#2 date, month_day) + 1
		  else get_month_day(#2 date, month_day)
	  in
	      #3 date >= 1 andalso #3 date <= max_day
	  end
  end
      
      
