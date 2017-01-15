let rec brackets n =
    if n >= 0 then (
        print_string "(";
        brackets (n-1);
        brackets (n-1);
        print_string ")";
    );;

let n = read_int();;
brackets n;;
