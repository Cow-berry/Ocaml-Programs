type br_tree = Br of br_tree list;;
(* │┌┐┬ ┴ ┼ ─ *)
let rec print_br_list brs =
  List.iter print_brackets brs;

and print_brackets (Br t)=
  print_string "Br[";
  print_br_list t;
  print_string "]"
     ;;

 let rec print_my_pair brs =
   List.iter (fun (shift, brs) ->
   print_string "(";
   print_int shift;
   print_string ", ";
   print_brackets brs;
   print_string ")";
   ) brs;;

let print_list f lst =
  let rec print_elements = function
    | [] -> ()
    | [h] -> f h;
    | h::t -> f h; print_string ";"; print_elements t
  in
  print_string "[";
  print_elements lst;
  print_string "]";;

let print_int_list = print_list print_int;;
let print_int_list_list = print_list print_int_list;;
let print_int_list_list_list = print_list print_int_list_list;;

let repeat_string s i =
  for i = 1 to i do
    print_string s;
  done;;

let rec size_tree (Br l) =
    if l = [] then 1
    else List.fold_left (+) 0 (List.map size_tree l);
  ;;

let dist_br shift br =
  shift + ((size_tree br) +1) / 2;;

let list_sizes (Br l) =
  List.map size_tree l;;

let rec sum l n =
  if n >= 0 then (List.nth l n) + (sum l (n-1))
  else 0
  ;;

let rec list_distances_rec sizes sum prev result =
match sizes with
| [] -> result
| h::t ->
  let elem = sum+(h+1)/2 in
  list_distances_rec t (sum+h) elem (result @ [elem - prev])
  ;;

let list_distances sizes=
  let abs_values = List.mapi (fun i x -> sum sizes (i-1) + (x+1)/2) sizes in
  List.mapi (fun i x -> if i = 0 then x else x - (List.nth abs_values (i-1))) abs_values
  ;;
let rec shift_tree shift l =
  match l with
  | [] -> []
  | h::t -> [(shift, h)] @ (shift_tree (shift+ size_tree h) t)
;;

let rec get_new_line brs prev=
print_string "get_new_line ";
print_my_pair brs;
print_string " prev = ";
print_int prev;
print_string "\n";
match brs with
| [] -> []
| (shift, Br[])::t ->  get_new_line t (prev)
| (shift, br)::t ->
        let last_distances = list_distances_rec (list_sizes br) shift prev [] in
        [
        last_distances
        ]
        @
        get_new_line t ( List.fold_left (+) prev last_distances)
;;

let rec observe_tree_rec brs current=
  print_string "current = ";
  print_int_list_list_list current;
  print_string "\n";
  print_string "brs = ";
  print_my_pair brs;
  print_string "\n";
  if brs = [] then current
  else (
  (* let new_line = (List.map (fun (shift, br) -> list_distances_rec (list_sizes br) shift 0 []) brs) in *)
  let new_line = get_new_line brs 0 in
  print_string "new_line = ";
  print_int_list_list new_line;
  print_string "\n";
  let new_trees = List.flatten  (List.map (fun (shift, (Br t))-> shift_tree shift t) brs) in
    observe_tree_rec new_trees (current @ [new_line])
  );;


let observe_tree br =
  print_string "Begin Observe\n";
  let return = observe_tree_rec [(0,Br[br])] [] in
  print_string "End Observe\n";
  return
  ;;
  (* [[[5]];[[2;4;3]];[[1;1;1;1];[1;1;1];[2]];[[8;1;1]]];; *)
     (* [[[5]];[[1;1;1;1];[1;1;1];[2]];[[];[];[];[];[];[];[];[1;1;1]];[[];[];[]]] *)
(* [
[[5]];
[[2;4;3]];
[[1;1;1;1];[1;1;1];[2]];
[[];[];[];[];[];[];[];[1;1;1]];
[[];[];[]]
]
 *)


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
        t
      ;;
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
  (* let br = (Br[Br[];Br[Br[Br[Br[Br[Br[Br[]]]]]];Br[];Br[];Br[];Br[];Br[]];Br[];Br[Br[];Br[];Br[Br[];Br[];Br[];Br[];Br[];Br[]];Br[];Br[];Br[]];Br[];Br[];Br[];Br[Br[];Br[];Br[];Br[Br[];Br[];Br[];Br[];Br[];Br[]];Br[];Br[]];Br[];Br[]]);; *)
(*        Br[Br[Br[] Br[] Br[] Br[]] Br[Br[] Br[] Br[]] Br[Br[Br[Br[]] Br[] Br[Br[]]]]] *)
 (* print_int_list_list_list (observe_tree br);; *)
(* let br = Br[Br[Br[Br[];Br[Br[]];Br[]]];Br[Br[Br[Br[Br[]]]]]];; *)

(* print_tree_h (Br[Br[Br[];Br[];Br[];Br[]];Br[Br[];Br[];Br[]];Br[Br[Br[];Br[];Br[]]]]);;

print_int (size_tree br);;
print_string "\n";;
print_int_list (list_sizes br);;
print_string "\n";;
print_int_list (list_distances (list_sizes br));;
print_string "\n";;
print_int_list (list_distances_rec (list_sizes br) 0 0 []);;
print_string "\n";;
print_int_list_list_list (observe_tree br);;
print_string "\n";; *)

print_distances (observe_tree br);;
print_string "\n";;
print_brackets br;;
print_string "\n";;
