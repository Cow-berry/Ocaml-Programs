let rec print_2 n =
    if n > 0 then print_2 (n-1);
    if n mod 2 = 1 then Printf.printf"%d " n;;

let n = read_int();;
print_2 n;;
print_string"\n";;
