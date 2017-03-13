print_string"\n";;
let g = -9.8;;
print_string "Введите начальную высоту : ";;
let h0 = read_float();;
print_string "Введите время, через которое хотите посмотреть положение : ";;
let t = read_float();;
(* x0+V0*t+(g*t^2)/2
он V0 = 0*)
let fall g h0 t =
  if (h0+.(g*.t*.t)/.2.)<0. then 0.
  else h0+.(g*.t*.t)/.2.;;

Printf.printf "%f\n\n" (fall g h0 t);;
