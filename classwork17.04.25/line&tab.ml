type brackets = Br of brackets list;;

let rec print_brackets br =
  List.iter (fun (Br(t)) ->
    print_string "(";
    print_brackets t;
    print_string ")") br ;;

let rec print_br prefix br =
  List.iteri (fun i (Br(t)) ->
    print_string (prefix ^ "*\n");
    if i =  (List.length br) -1 then
      print_br (prefix ^ "  ") t
    else
      print_br (prefix ^ "|-") t;
  ) br ;;


 let repeat_string s i =
  for i = 1 to i do
    print_string s;
  done;;

let rec print_tab br i =
  List.iter (fun (Br(t)) ->
    print_string(" ");
    print_tab t (i+1);
    print_string ("\n"));;

let rec print_brackets_depth br i =
  List.iter (fun (Br(t)) ->
    print_int i;
    print_string "(";
    print_brackets_depth t (i+1);
    print_string ")") br ;;


let b = [Br[];Br[Br[Br[];Br[];Br[]];Br[Br[];Br[];Br[]];Br[Br[];Br[];Br[]]];Br[Br[];Br[];Br[]]];;
(* print_br "" b;; *)
print_br "" b;;
(* print_tab b;; *)
