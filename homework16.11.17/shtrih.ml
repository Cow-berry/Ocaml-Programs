let rec shtrih n =
    if n > 0 then shtrih (n-1);
    print_string "* ";;

let n = read_int();;
shtrih n;;
print_string "\n";;
