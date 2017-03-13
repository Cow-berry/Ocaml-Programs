let x = [1;2;3;4;5];;
let y = [1;2;3;4;5];;

let rec print_pairList l =
  List.iter (fun (x, y) -> Printf.printf "(%d, %d);" x y) l;;


let dekart x y =
  List.concat(
    List.map(fun x ->
        List.map(fun y -> (x, y)) y) x);;

print_string"\n";;
print_pairList(dekart x y);;
