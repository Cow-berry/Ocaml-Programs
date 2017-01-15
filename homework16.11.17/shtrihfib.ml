let rec fib a =
    if a < 2 then 1
    else fib (a-1) + fib (a-2);;

let rec repeat_char n str =
    if n > 1 then repeat_char (n-1) str;
    Printf.printf  "%s" str;;

let rec shtrih n =
    if n >1 then shtrih (n-2);
    repeat_char (fib n) "*";
    repeat_char (fib (n+1)) " ";;

repeat_char 3 "/"
let n = read_int();;
if n mod 2 = 1 then shtrih (n-1)
else shtrih n;;
print_string "\n";;
