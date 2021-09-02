fun is_older(x: int*int*int, y: int*int*int) = 
  (* true is x comes first *)
  if (#1 x) = (#1 y) 
  then 
    if (#2 x) = (#2 y)
    then 
      if (#3 x) = (#3 y)
      then false
      else (#3 x) < (#3 y)
    else (#2 x) < (#2 y)
  else (#1 x) < (#1 y)

(* Write a function number_in_month  *)
(* that takes a list of dates and a month (i.e., an int)  *)
(* and returns how many dates in the list are in the given month *)

fun number_in_month(dates: (int*int*int) list, month: int) =
  if null dates then 0
  else
    let val date = hd dates
    in 
      if #2 date = month
      then 1 + number_in_month(tl dates, month)
      else 0 + number_in_month(tl dates, month)
    end

val x = number_in_month([(1, 8, 3), (2, 8, 3), (4, 1, 9)], 8);

(* Write a function number_in_months  *)
(* that takes a list of dates and a list of months (i.e., an int list) and  *)
(* returns the number of dates in the list of dates  *)
(* that are in any of the months in the list of months.  *)

(* Assume the list of months has no number repeated. Hint: Use your answer to the *)
(* previous problem.  *)
fun number_in_months(dates: (int*int*int) list, months: int list) =
  if null dates then 0 
  else
    if null months then 0 
    else
      number_in_month (dates, hd months) + 
      number_in_months(dates, tl months)



fun dates_in_month(dates: (int*int*int) list, month: int) =
  if null dates then []
  else 
    if (#2 (hd dates)) = month
    then (hd dates) :: dates_in_month(tl dates, month)
    else dates_in_month(tl dates, month)



fun dates_in_months(dates: (int*int*int) list, months: int list) =
  if null dates then []
  else 
    if null months
    then []
    else 
      dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)


fun get_nth(l: string list, n: int) =
  if n = 1 then hd l
  else get_nth(tl l, n-1)


fun int_to_month(month: int) = 
  let val months = ["January", "February", "March", "April", 
                    "May", "June", "July", "August", 
                    "September", "October", "November", "December"]
  in
    get_nth(months, month)
  end

fun date_to_string(date: int*int*int) =
    int_to_month(#2 date) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)

fun number_before_reaching_sum(sum: int, vals: int list) = 
  let fun helper(count: int, total: int, sum: int, vals: int list) = 
    if total + hd vals >= sum
    then count
    else helper(count+1, total + hd vals, sum, tl vals)
  in helper(0, 0, sum, vals)
  end

val x = number_before_reaching_sum(14, [2, 2, 3, 8, 5])

fun what_month(doy: int) =
  let val b = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
  in
    number_before_reaching_sum(doy, b) + 1
  end
