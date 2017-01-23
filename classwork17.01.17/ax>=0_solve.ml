type solution = R
  |More
  |Less;;

let solve a =
  match (a<0., a = 0.) with
    (true, false) -> Less
    |(false, true) -> R
    |(_, _) -> More;;

let print_solve s =
  match s with
    Less -> print_string "x < 0.\n"
    |R -> print_string "x - любое.\n"
    |More -> print_string "x > 0.\n";;

let a = read_float();;
let s = solve a;;
print_solve s;;
