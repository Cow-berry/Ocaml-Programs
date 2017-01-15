let rec one_two n =
    if n > 1 then one_two (n-1);
    if n mod 3 <> 0 then Printf.printf "%d " n;;

let n = read_int();;
one_two n;;
print_string"\n";;
