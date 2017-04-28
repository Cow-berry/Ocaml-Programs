type brackets = Br of brackets list;;

let rec print_brackets br =
  List.iter (fun (Br(t)) ->
    print_string "(";
    print_brackets t;
    print_string ")") br ;;

print_brackets [Br[Br[]];Br[]];;
print_string"\n";;
