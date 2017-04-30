type br_tree = Br of br_tree list;;

let rec iter_last f1 f2 l =
  match l with
    [] -> ()
    | [x] -> f2 x
    | h::t -> f1 h;
              iter_last f1 f2 t;;

let print_path q =
  iter_last (fun x -> print_string(if x then "│" else "  "))
            (fun x -> print_string(if x then "├" else "└")) q;;

let rec print_br_list t path =
  iter_last (fun x -> print_br x (path @ [true]))
            (fun x -> print_br x (path @ [false])) t
  and print_br (Br t) path =
      print_path path ; print_string"*\n";
      print_br_list t path;;

let br = (Br[Br[Br[];Br[];Br[];Br[Br[Br[Br[Br[]]]]]];Br[Br[Br[Br[Br[]];Br[];Br[Br[Br[Br[Br[]];Br[];Br[Br[]]]]]]];Br[Br[Br[Br[]];Br[];Br[Br[]]]];Br[Br[Br[Br[]];Br[];Br[Br[]]]]];Br[Br[Br[Br[]];Br[];Br[Br[]]]]]);;

(* print_br (Br[Br[Br[Br[];Br[]];Br[Br[];Br[]]];Br[Br[Br[];Br[]];Br[Br[];Br[]]]]) [];; *)
(* print_br (Br[Br[Br[];Br[];Br[];Br[]];Br[Br[];Br[];Br[]];Br[Br[Br[];Br[];Br[]]]]) [];; *)
print_br br [];;
