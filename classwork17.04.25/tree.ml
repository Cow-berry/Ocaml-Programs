type br_tree = Br of br_tree list;;


let print_path p =
  List.iter ( fun x ->
    print_string(if x then "|"
                else " ")p)

let rec print t path =
  print_path path;
  print_string"*\n";;
  List.iter (lj)

  дз
