type solution = None |
  One of float|
  R;;

let solve a b =
  match (a=0., b=0.) with
    (true, true) -> R
    |(true, false) -> None
    |(false, x) -> One(-.b/.a);;

let print_solution s =
  match s with
    None -> print_string "None."
    |R -> print_string "Infinity."
    |One x -> print_string "{";
              print_float x;
              print_string "}";;

let solve_s a b c =
  solve a (b-.c);;


let x = solve 2. 3.;;
print_solution x;;
