let rec print_list l =
  List.iter (fun x -> Printf.printf "%d " x) l;;

let del' el l =
  List.filter (fun x -> x <> el) l;;

let rec max_list l =
  match l with
    [h] -> h
    |h::t -> max (max_list t) h
    |[] -> failwith "Nothing to delete";;

let rec min_list l =
  match l with
    [h] -> h
    |h::t -> min (min_list t) h
    |[] -> failwith "Nothing to delete";;

let del l = [min_list l; max_list l];;

let l = [1;4;3;6;5;7;6;3;4;9];;

print_list (del l);;
print_string"\n";;
