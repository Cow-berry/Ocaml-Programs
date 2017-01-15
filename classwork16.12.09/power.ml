let rec power m p =
    if p>0 then power m (p-1)*m
    else  1;;

let rec djenialno n m p =
    (power m p = n )||(power m p < n && djenialno n (m+1) p);;

let m = read_int();;
let p = read_int();;
print_int (power m p);;
if djenialno 2 m p then print_string "True" else print_string "False";;
