(*let rec A m n =
    if m > 0 then n+1
    else if (m > 0) && (n = 0) then A (m-1) 1
    else if (m > 0) && (n > 0) then A (m-1) (A m (m-1));;
*)
let rec akk m n =
    match (m, n) with
    (0, n) -> n+1
    | (m, 0) -> akk (m-1) 1
    | (m, n) -> akk (m-1) (akk m (n-1));;

let rec akkN m n k=
    if (akk m n >= k) && (akk m (n-1) < k) then n
    else akkN m (n+1) k;;

let n = read_int();;
let m = read_int();;
print_int(akk n m);;
let m = read_int();;
let k = read_int();;
print_int (akkN m 0 k);
