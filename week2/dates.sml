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


(* 7. Writea function date_to_string that takes a date and returns a string of
        * the form  January 20, 2013 (for example).  *)

(* Use the operator ^ for concatenating strings and the *)
(* library function Int.toString for converting an int to a string. For producing *)
(* the month part, do not use a bunch of conditionals. Instead, use a list holding *)
(* 12 strings and your answer to the previous problem. For consistency, put a comma *)
(* following the day and use capitalized English month names: January, February, *)
(* March, April, May, June, July, August, September, October, November, December. *)

(* fun date_to_string(date: int*int*int date) = *)

fun dates_in_month(dates: (int*int*int) list, month: int) =
  if null dates then []
  else 
    if (#2 (hd dates)) = month
    then (hd dates) :: dates_in_month(tl dates, month)
    else dates_in_month(tl dates, month)

val x = dates_in_month([(1, 4, 3), (7, 2, 5)], 4)
val y = dates_in_month([], 4)


fun dates_in_months(dates: (int*int*int) list, months: int list) =
  if null dates then []
  else 
    if null months
    then []
    else 
      dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)

val y = dates_in_months([(1, 4, 3), (7, 2, 5)], [5, 2])

(* 6. Write a function get_nth that takes a list of strings and an int n and *)
(* returns the nth element of the list where the head of the list is 1st. Do not *)
(* worry about the case where the list has too few elements: your function may *)
(* apply hd or tl to the empty list in this case, which is okay. *)

fun get_nth(l: string list, n: int) =
  if n = 1 then hd l
  else get_nth(tl l, n-1)
