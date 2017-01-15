let rec sum_1 n =
    if n mod 2 = 0 then sum_1 (n-1)
    else if n >= 1 then n+sum_1 (n-2)
    else 0;;

let n = read_int();;
print_int (sum_1 n);;
print_string"\n";;
