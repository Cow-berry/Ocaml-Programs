(*
a:b для 1...n

*)


let m = [1;2;3;4;5;6;7;8]
(* 0 *)
let print_str n l m =
  for i = 1 to List.length m do
    if List.mem (n, i) l then print_int 1
    else print_int 0 done;;

let print_rel m l =
  for i = 1 to List.length m do
    print_str i l m ;
    print_string "\n" done;;

print_rel m [(1, 1);(2, 2);(2, 8)];;
(* 3 *)

(*l = [];;
let square n =
    square' n (n-1) [];;

let rec square' n  m l =
    if m > 1 then square' n (m-1);
    square'' n m l;

let rec square'' n m l
    l::

let n = read_int();;
square n;;
*)
