let rec power_4 n =
    if n > 1 then power_4 (n-1);
    Printf.printf "%d " (n*n*n);;

let n = read_int();;
power_4 n;;
print_string"\n";;
