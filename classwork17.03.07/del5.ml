let rec print_list l =
  List.iter (fun x -> Printf.printf "%d " x) l;;

let del2 l =
  List.filter (fun x -> x mod 5 <> 0) l;;


let l = [1;2;3;4;5;6;7;8;9;10;15;21];;
print_list(del2 l);;
print_string"\n";;
