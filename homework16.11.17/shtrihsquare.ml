let rec repeat_char n str =
    if n > 1 then
    (
    repeat_char (n-1) str;
    Printf.printf  "%s" str
    );;

let rec into n k=
    if k > 1 then into n (k-1);
    print_string "*";
    repeat_char (n-k-1) " ";
    repeat_char (k+1) "*";
    print_string "\n";;

let square n =
    repeat_char n "*";
    print_string "\n";
    into n (n-2);
    repeat_char n "*";
    print_string "\n";;

let n = read_int();;
square n;;
