type brackets = Br of brackets list;;

let repeat_tabs i =
  for i = 1 to i do
    print_string"  ";
  done;;
(*
let rec print_tab' br i =
  match br with
    Br[] -> ()
    |h::t ->
      if i > 0 then
        repeat_string "    " i;
      print_tab h (i+1);
      print_tab t 0;;

let rec print_brackets br =
  List.iter (fun (Br(t)) ->
    print_string "(";
    print_brackets t;
    print_string ")") br ;;*)

let rec print_tab br i =
  List.iter (fun (Br(t)) ->
    if i > 0 then
      repeat_tabs i;
    print_string "*\n";
    print_tab t (i+1)) br;;

print_tab [Br[Br[Br[]]];Br[]] 0;;
