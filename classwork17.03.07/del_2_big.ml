let rec max_list l =
  match l with
    [h] -> h
    |h::t -> max (max_list t) h
    |[] -> failwith "Nothing to delete";;

let del' el l =
  List.filter (fun x -> x <> el) l;;

let del l = del' (max_list (del' (max_list l)  l)) (del' (max_list l)  l);;

let rec print_list l =
  List.iter (fun x -> Printf.printf "%d " x) l;;

let l = [1;5;6;6;7;8;8];;
print_list (del l);;
print_string"\n"
