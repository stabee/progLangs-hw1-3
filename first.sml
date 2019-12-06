fun is_older(d1: int*int*int, d2: int*int*int) =
    if (#1 d1 < #1 d2)
    then true
    else if (#2 d1 < #2 d2)
    then true
    else if (#3 d1 < #3 d2)
    then true
    else false

fun number_in_month (dates : (int*int*int) list, month : int) =
    if (null dates)
    then 0
    else if (#2(hd(dates)) = month)
        then 1 + number_in_month(tl(dates), month)
        else number_in_month(tl(dates), month)

fun number_in_months (dates: (int*int*int) list, months: int list) =
    if (null months)
    then 0
    else number_in_month(dates, hd months) + number_in_months(dates, tl months);

fun dates_in_month(dates: (int*int*int) list, month: int) = 
    if (null dates)
    then []
    else if (#2(hd(dates)) = month)
        then hd(dates)::dates_in_month(tl dates, month)
        else dates_in_month(tl dates, month)

fun dates_in_months(dates: (int*int*int) list, months: int list) =
    if (null months)
    then []
    else dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)

fun get_nth(strings: string list, n: int) =
    let fun helper_fun(string_helper: string list, count) =
        if (count = n)
        then hd string_helper
        else helper_fun(tl string_helper, count + 1)
    in
        helper_fun(strings, 1)
    end

fun date_to_string(date: (int*int*int)) =
    let val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
    in
        let val month_to_string = get_nth(months, #2 date)
        in month_to_string ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
        end
    end

fun number_before_reaching_sum(sum: int, num_list: int list) =
    let fun sum_helper(n_list: int list, count: int, sum_so_far: int) =
        if (sum_so_far < sum)
        then sum_helper(tl n_list, count + 1, sum_so_far + hd n_list)
        else count - 2
    in
        sum_helper(num_list, 1, 0)
    end

fun what_month(day: int) =
    let val cut_offs = [0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365]
    in
        number_before_reaching_sum(day, cut_offs)
    end

fun month_range(day1: int, day2: int) =
    if (day1 > day2)
    then []
    else what_month(day1) :: month_range(day1 + 1, day2)

fun oldest(dates: (int*int*int) list) = 
    if null dates
    then NONE
    else let fun traverse_list(oldest_date: (int*int*int), remaining_dates: (int*int*int) list) =
        if null remaining_dates
        then SOME remaining_dates
        else if (is_older(oldest_date, hd remaining_dates))
            then traverse_list(oldest_date, tl remaining_dates)
            else traverse_list(hd remaining_dates, tl remaining_dates);
        in
            traverse_list(hd dates, tl dates)
        end