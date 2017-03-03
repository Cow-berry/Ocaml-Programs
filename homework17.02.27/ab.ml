let a = [(1, 7);(2, 4);(3, 5)];;

let print_list l =
  print_string "[";
  print_int (List.nth l 0);
  for i = 1 to List.length l -1 do
    print_string " ";
    print_int(List.nth l i) done;
  print_string "]";;

let rec b a =
  match a with
    []->[]
    |h::t-> (fst h)::(snd h)::(b t);;

print_list (b a);;
