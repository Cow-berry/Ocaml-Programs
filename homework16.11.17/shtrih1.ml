let rec repeat_char n str =
    if n > 1 then repeat_char (n-1) str;
    Printf.printf  "%s" str;;

let rec shtrih n =
    if n > 1 then shtrih (n-1);
    repeat_char n "*";
    repeat_char n " ";;

let n = read_int();;
shtrih n;;
print_string "\n";;
