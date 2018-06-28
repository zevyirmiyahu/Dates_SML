(*
Author: Zev Yirmiyahu
E-mail: zy@zevyirmiyahu.com
Personal site: zevyirmiyahu.com

A simple ML project to calculate various information about dates.

 NOTE: a date is defined here to have the form  year*month*day -> int*int*int *)


(* compares two dates and return true if first date is older than second date *)
fun is_older(firstDate : int*int*int, secondDate : int*int*int) =
    if #1 firstDate < #1secondDate
    then true
    else if #1 firstDate = #1 secondDate andalso #2 firstDate < #2 secondDate
    then true
    else if #1 firstDate = #1 secondDate andalso #2 firstDate = #2 secondDate andalso #3 firstDate < #3 secondDate
    then true
    else false

(* Takes a list of dates and a month and finds number of times that month appears in list of dates *)
fun number_in_month(dates : (int * int * int) list, month : int) =
	if null dates
	then 0
	else if #2(hd dates) = month
	then 1 + number_in_month(tl(dates), month)
	else number_in_month(tl dates, month)

(* Takes a list of dates and list of months counts how many
times each month appears in the list of dates
NOTE: assumes months do NOT repeat in list of months *)
fun number_in_months(dates : (int*int*int) list, months : (int) list) =
    if null dates orelse null months
    then 0
    else
	number_in_month(dates, hd months)
	+ number_in_months(dates, tl months)

(* list holding the dates from the argument list of dates that are in the month *)
fun dates_in_month(dates : (int*int*int) list, month : int) =
    if null dates
    then dates
    else if (#2(hd dates)) = month
    then (hd dates)::dates_in_month(tl dates, month)
    else dates_in_month(tl dates, month)
		       
(* NOTE: Assumes the list of months has no number repeated *)					 
fun dates_in_months(dates : (int*int*int) list, months : (int) list) =
    if null dates orelse null months
    then []
    else
	dates_in_month(dates, hd months)@dates_in_months(dates, tl months) 

fun get_nth(xs : string list, n : int) =
    if n = 1
    then hd xs
    else get_nth(tl xs, n - 1)
						

fun date_to_string(date : (int*int*int)) =
    let	
	val dateList = ["Janaury", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
	val year = #1(date)
	val month = #2(date)
	val day = #3(date) 
    in
	get_nth(dateList, month)^" "^Int.toString(day)^", "^Int.toString(year)
    end

(* Returns a total of ints from the list that are less then the designated sum *)
fun total_of_numbers_before_reaching_sum(sum : int, xs : int list) =
    if null xs
    then 0
    else
	let
	    val total = hd xs
	in
	    if null (tl xs)
	    then 0
	    else if total <= sum 
	    then total + total_of_numbers_before_reaching_sum(sum - total, tl xs)
	    else total - hd xs
	end

	    
(* Finds how many numbers in a list before those elements in the list add to sum, which is specfied by the user *)
fun number_before_reaching_sum(sum : int, xs : int list) = 
    if null xs
    then 0
    else if null (tl xs) andalso sum < hd xs
    then 0
    else if null (tl xs)
    then 1
    else if hd xs > sum
    then 0
    else if hd xs + hd(tl xs) < sum 
    then 1 + number_before_reaching_sum(sum - hd xs, tl xs)
    else 1

	     
(* Used to count the proper index for what_month function *)
fun counter(days_in_month : int list, day : int) =
    if null days_in_month
    then 0	 
    else if day > 0
    then 1 + counter(tl days_in_month, day - hd days_in_month)
    else 0
  
   
(* Takes a day of year and returns the name of that month that day is in. For example, the 142th day of the year appears in the month of May. *NOTE* excludes leap years in calculation *)
fun what_month_name(day : int) =
    let
	val months = ["Janaury", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
	val days_in_month = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
	val index = counter(days_in_month, day)
    in
	get_nth(months, index)				   
    end


(* converts month name to month index number *)
fun month_index(month_name : string) =
    
    	if month_name = "Janaury"
	then 1
	else if month_name = "February"
	then 2
	else if month_name = "March"
	then 3
	else if month_name = "April"
	then 4
	else if month_name = "May"
	then 5
	else if month_name = "June"
	then 6
	else if month_name = "July"
	then 7
	else if month_name = "August"
	then 8
	else if month_name = "September"
	then 9
	else if month_name = "October"
	then 10
	else if month_name = "November"
	then 11
	else 12	

	
(* Take a day of the year and returns the numerical number for that month that the day is in. *)
fun what_month(day: int) =

    let 
	val month_name = what_month_name(day)
    in
	month_index(month_name)
    end
		 

(* Take a first day and a second day and determines what months are contained within that range of dates including the start and end day *)
fun month_range(day1 : int, day2 : int) =
    if day1 > day2 
    then []
    else
	let
	    val dayN = day1
	    val months = ["Janaury", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
	    val days_in_month = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
	    val index = counter(days_in_month, dayN)
	in
	    if dayN <> day2
	    then index::month_range(dayN + 1, day2)
	    else if dayN = day2
	    then index::[]
	    else []
	end

	    
(* Takes in a list of dates and returns the oldest date in the list *)
fun oldest(dates : (int*int*int) list) =
    if null (tl dates)
    then dates
    else if null dates
    then [(0,0,0)]
    else if is_older(hd dates, hd(tl dates))
    then oldest((hd dates)::(tl(tl dates)))
    else oldest(tl dates)

