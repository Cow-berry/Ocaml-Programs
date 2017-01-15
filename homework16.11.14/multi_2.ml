let rec multi_2 n =
    if n mod 2 = 1 then multi_2 (n-1)
    else if n >= 2 then n*multi_2 (n-2)
    else 1;;

let n = read_int();;
print_int (multi_2 n);;
print_string"\n";;
