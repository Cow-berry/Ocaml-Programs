let l = [2;3;5;1;7;1];;

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


let rec print_list l =
  List.iter (fun x -> Printf.printf "%d " x) l;;

let del el l =
  List.filter (fun x -> x <> el) l;;

let del l = del (min_list l) (del (max_list l)  l);;

Printf.printf "min_list (l) = %d\n" (min_list l);;
Printf.printf "max_list (l) = %d\n" (max_list l);;

print_list (del l);;
print_string"\n";;
