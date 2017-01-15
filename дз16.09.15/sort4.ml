let max a b = if a >= b then a else b;;
let min a b = if a <= b then a else b;;

let max3 a b c = if a >= c then max a b else max b c;;
let min3 a b c = if a <= c then min a b else min b c;;

let min4 a b c d = if a <= d then min3 a b c else min3 b c d;;
let max4 a b c d = if a >= d then max3 a b c else max3 b c d;;

let med3 a b c = min3 (max a b) (max b c) (max a c);;
let med4pl a b c d = max3 (med3 a b c) (med3 b c d) (med3 d a b);;
let med4mn a b c d = min3 (med3 a b c) (med3 b c d) (med3 d a b);;

let a = read_int();;
let b = read_int();;
let c = read_int();;
let d = read_int();;
print_string "\n";;
print_int (max4 a b c d);;
print_string ", ";;
print_int (med4pl a b c d);;
print_string ", ";;
print_int (med4mn a b c d);;
print_string "\n";;
