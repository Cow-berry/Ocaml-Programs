let l = [1;2;3;4;5;6;7;8;9;10];;
let rec print_list l =
  List.iter (fun x -> Printf.printf "%d " x) l;;

let everySnd l =
  List.fold_left (fun x y ->
    if List.nth x 1 = 0 then [(List.nth x 0)@([y]), 1]
    else [x, 0]) [[], 1] l;;


print_list (snd(everySnd l));;
