type br_tree = Br of br_tree list;;
(*
спецсимволы : │┌ ┐┬ ─
*)

let repeat_string s i =
  for i = 1 to i do
    print_string s;
  done;;

let rec size_tree (Br l) =
    if l = [] then 1
    else List.fold_left (+) 0 (List.map size_tree l);;

let dist_br shift br =
  shift + ((size_tree br) +1) / 2;;

let list_sizes (Br l) =
  List.map size_tree l;;

let rec sum l n =
  if n >= 0 then (List.nth l n) + (sum l (n-1))
  else 0;;

let rec list_distances_rec sizes sum prev result =
match sizes with
| [] -> result
| h::t ->
  let elem = sum+(h+1)/2 in
  list_distances_rec t (sum+h) elem (result @ [elem - prev]);;

let rec shift_tree shift l =
  match l with
  | [] -> []
  | h::t -> [(shift, h)] @ (shift_tree (shift+ size_tree h) t);;

let rec get_new_line brs prev=
match brs with
  | [] -> []
  | (shift, Br[])::t ->  get_new_line t (prev)
  | (shift, br)::t ->
                let last_distances = list_distances_rec (list_sizes br) shift prev [] in
                [last_distances] @ get_new_line t ( List.fold_left (+) prev last_distances);;

let rec observe_tree_rec brs current=
  if brs = [] then current
  else (
  let new_line = get_new_line brs 0 in
  let new_trees = List.flatten  (List.map (fun (shift, (Br t))-> shift_tree shift t) brs) in
    observe_tree_rec new_trees (current @ [new_line])
    );;


let observe_tree br =
  observe_tree_rec [(0,Br[br])] [];;


let rec iter_last f1 f2 l =
    match l with
      [] -> ()
      | [x] -> f2 x
      | h::t -> f1 h;
                iter_last f1 f2 t;;

let print_edge distances =
  match distances with
  | [] -> ()
  | [d] -> repeat_string " " (d-1); print_string "│"
  | d::t ->
      repeat_string " " (d-1);print_string "┌";
      iter_last
        (fun d -> repeat_string "─" (d-1);print_string "┬";)
        (fun d -> repeat_string "─" (d-1);print_string "┐";)
        t;;

let print_edges distances2 =
  List.iter (fun distances -> print_edge distances) distances2;
  print_string "\n";;

let print_stars distances =
    List.iter( fun d -> repeat_string " " (d-1); print_string "*" ) distances;
    print_string "\n";;

let print_distances distances3=
  match distances3 with
  | [] -> ()
  | d2::t ->
    print_stars (List.flatten d2);
    List.iter (fun distances2 -> print_edges distances2; print_stars (List.flatten distances2)) t;;

let rec print_tree_h br =
  let distances3 = observe_tree br in
  print_distances distances3;;

let br = (Br[Br[Br[];Br[];Br[];Br[]];Br[Br[Br[Br[Br[]];Br[];Br[Br[Br[Br[Br[]];Br[];Br[Br[]]]]]]];Br[Br[Br[Br[]];Br[];Br[Br[]]]];Br[Br[Br[Br[]];Br[];Br[Br[]]]]];Br[Br[Br[Br[]];Br[];Br[Br[]]]]]);;


print_distances (observe_tree br);;
print_string "\n";;
