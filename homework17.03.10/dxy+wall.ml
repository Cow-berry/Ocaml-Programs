print_string "Введите начальный x : ";;
let x = read_int();;
print_string "Введите начальный y : ";;
let y = read_int();;
print_string "Введите движение по x : ";;
let dx = read_int();;
print_string "Введите движение по y : ";;
let dy = read_int();;
print_string "Введите координату вертикальной стены : ";;
let x0 = read_int();;
print_string "Введите количество итераций : ";;
let n = read_int();;
let dxy' x y dx dy x0 n =
  if ((x < x0 ) && (x0 < x+n*dx))|| ((x > x0) && (x0 > x+n*dx)) then ((2*x0-x-n*dx), (y+n*dy))
  else ((x+dx*n), (y+dy*n));;

let print_pair p = Printf.printf "(%d, %d)\n" (fst p) (snd p);;
print_pair (dxy' x y dx dy x0 n);;
