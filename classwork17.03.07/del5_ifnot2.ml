let rec print_list l =
  List.iter (fun x -> Printf.printf "%d " x) l;;

let del' el l =
  List.filter (fun x -> x <> el) l;;

let del l =
  if List.mem 2 l then del' 5 l
  else l;;

let l = [1;3;4;5;2;5;5;6;7;5];;
print_list (del l);;
print_string"\n";;
