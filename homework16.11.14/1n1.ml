let rec up n =
    if n > 1 then up (n-1);
    Printf.printf "%d " n;;

let rec down n =
    Printf.printf "%d " n;
    if n > 1 then down (n-1);;

let up_down n =
    up n;
    down (n-1);;

let n = read_int();;
up_down n;;
print_string"\n";;
