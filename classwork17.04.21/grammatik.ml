type brackets = Br of brackets list;;

(* Br []
Br [Br []; Br [Br []]] *)

let rec print_brackets br =
  List.iter (fun (Br(t)) ->
    print_string "(";
    print_brackets t;
    print_string ")") br ;;


let rec depth d=
  List.fold_left max 0 (List.map (fun (Br b) -> depth b)d) +1;;

print_brackets [Br[Br[];Br[Br[]]]];;
print_int (depth [Br[Br[];Br[Br[]]]]);;
print_string "\n";;
(*
4
*)
let rec count_pair br =
    depth br + 1;;(*прочто подсчёт глубины с добавлением внешней скобки*)
(*
1) тип скобки, depth , print DONE
2) Построить полное дерево скобок :
  ветвление по 3 , глубины k
3) Посчитать самую большую степень вершины
4) посчитать к-во пар скобок DONE
5) проверить что в дереве степени меньше k
*)
