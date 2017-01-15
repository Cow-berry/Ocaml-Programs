let rec fib a =
    if a < 2 then 1
    else fib (a-1) + fib (a-2);;

let rec loop n =
    if n > 0 then
    begin
    loop (n-1);
    print_int (fib n);
    print_string "\n"
    end;;

let a = read_int();;
loop a
