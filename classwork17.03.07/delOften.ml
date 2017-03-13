let rec print_list l =
  List.iter (fun x -> Printf.printf "%d " x) l;;

let rec print_pairList l =
  List.iter (fun (x, y) -> Printf.printf "(%d, %d) " x y) l;;

let del'' el l =
  List.filter (fun x -> x <> el) l;;

let del' el l =
  List.filter (fun x -> x = el) l;;

let count'' el l =
  List.length (del' el l);;

let count' l =
  List.map (fun x -> (x, (count'' x l))) l;;

let rec max_often c =
  if List.length c > 0 then max (snd(List.nth c 0)) (max_often (del'' (List.nth c 0) c))
  else 0;;


let l' = [1;2;3;3;4;5;5;5;5;5;5;6;7];;
let l = List.sort l';;
let lu = List.sort_uniq l;;
print_list l;;
print_string "\n";;
print_pairList(count' l);;
print_string "\n";;
print_pairList(count l);;
print_string "\n";;
print_int (max_often (count' l));;
