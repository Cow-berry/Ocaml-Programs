let rec gcd a b =
    if a<b then gcd b a
    else if (b=0) then a
    else gcd b (a mod b) ;;

let a = read_int ();;
let b = read_int ();;
print_int (gcd a b)
