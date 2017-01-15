let rec power x n =
    if n > 0 then
    x * (power x (n-1))
    else 1;;

let rec sumx x n =
    if n = 0 then 1
    else
    (power x n) + (sumx x (n-1));;

let x = read_int();;
let n = read_int();;

print_int(sumx x n);;
