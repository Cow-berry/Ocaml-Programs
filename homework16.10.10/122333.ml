let rec repeat_int n symbol =
    if n>0 then (
        print_int symbol;
        repeat_int (n-1) symbol
    );;

let rec int n =
    if n > 0 then (
        int(n-1);
        repeat_int n n;
        print_string " ";
    );;

let n = read_int();;
int n;;
