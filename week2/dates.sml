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
    else dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)


fun get_nth(l: string list, n: int) =
  if n = 1 then hd l
  else get_nth(tl l, n-1)

fun int_to_month(month: int) = 
  let val months = ["January", "February", "March", "April", 
                    "May", "June", "July", "August", 
                    "September", "October", "November", "December"]
  in get_nth(months, month) end

fun date_to_string(date: int*int*int) =
    int_to_month(#2 date) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)

fun number_before_reaching_sum(sum: int, vals: int list) = 
  let fun helper(count: int, total: int, sum: int, vals: int list) = 
    if total + hd vals >= sum
    then count
    else helper(count+1, total + hd vals, sum, tl vals)
  in helper(0, 0, sum, vals)
  end

fun what_month(doy: int) =
  let val b = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
  in
    number_before_reaching_sum(doy, b) + 1
  end

fun month_range(day1: int, day2: int) = 
  if day1 > day2 then [] else
  let fun appender(day1: int, day2: int, months: int list) = 
    if day1 <= day2
    then appender(day1, day2-1, what_month(day2)::months)
    else months
  in
    appender(day1, day2, [])
  end

