let rec repeat_char n str =
    if n > 1 then
    (
    repeat_char (n-1) str;
    Printf.printf  "%s" str
    );;

let rec fib a =
    if a < 2 then 1
    else fib (a-1) + fib (a-2);;

let fibSprouse_in n k =
    repeat_char ((fib (n)*2 - fib(k))/2) " ";
    if k = 1 then repeat_char (fib(k)) "*";
    if k =2 then repeat_char 3 "*"
    else repeat_char (fib(k)*2) "*";
    repeat_char ((fib (n)*2 - fib(k))/2) " ";
    print_string"\n";;

let rec fibSprouse n k =
    if k > 1 then fibSprouse n (k - 1);
    fibSprouse_in n k;;

let n = read_int();;
fibSprouse n n;;
