type solution = None
  |Onem of float
  |Onel of float
  |All;;

let solve a b =
  match (a=0., b=0., b<0., a<0.) with
    (true, true, false, false) -> All
    |(true, false, true, false) -> None
    |(false, _, _, true) -> Onel (-.b/.a)
    |(_, _, _, _) -> Onem (-b/.a);;

let print_solution s =
  match s with
    None -> print_string"Нет решения.\n"
    |Onel x -> Printf.printf "(Infinity, %f]\n" x
    |Onem x -> Printf.printf "[%f, Infinity)\n" x
    |All -> print_string "х-любое.\n";;

let a = read_float();;
let b = read_float();;
let s = solve a b;;
print_solution s;;
