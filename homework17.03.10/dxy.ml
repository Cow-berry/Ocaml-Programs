print_string "Введите начальный x : ";;
let x = read_int();;
print_string "Введите начальный y : ";;
let y = read_int();;
print_string "Введите движение по x : ";;
let dx = read_int();;
print_string "Введите движение по y : ";;
let dy = read_int();;
print_string "Введите количество итераций : ";;
let n = read_int();;

let dxy x y dx dy n = ((x+dx*n), (y+dy*n));;

let print_pair p = Printf.printf "(%d, %d)\n" (fst p) (snd p);;

print_string "Конечные координаты : ";;
print_pair (dxy x y dx dy n);;
