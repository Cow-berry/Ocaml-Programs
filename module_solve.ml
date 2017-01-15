type solution =
  None
  |One of float
  |Two of (float*float);;

let solve a =
  match (a<0., a=0.) with
    (true, false) -> None
    | (false, true) -> One(a)
    | (_, _) -> Two(a, -.a)
    ;;

let print_solution s =
  match s with
    None -> print_string"Уравнение не имеет решений.\n"
    |One x -> print_string "{";
             print_float x;
             print_string "}\n"
    |Two (x, y) -> print_string "{";
             print_float x;
             print_string ", ";
             print_float y;
             print_string "}\n";;

let a = read_float();;
let s = solve a;;
print_solution s;;
