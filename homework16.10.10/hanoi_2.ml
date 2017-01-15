let rec move src dst aux n =
    if n = 0 then ()
    else (
        move src aux dst (n-1);
        Printf.printf "%s -> %s\n" src dst;
        move aux dst src (n-1)
    );;

let rec hanoi_max src dst aux n c =
    if n = 0 then ()
    else (
        if src != "A" && aux !="A" then
            hanoi_max src aux dst (n-1) (c+1);
        if aux != "A" && dst !="A" then
            hanoi_max aux dst src (n-1) (c+1);
        print_int c;
        print_string "\n";
    );;

let n = read_int();;
hanoi_max "A" "B" "C" n 0;;
