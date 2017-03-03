let l = [1;5;5;3;2;5;2;3;6;9;8;7;4;1;2;5];;

let cht x =
  if x mod 2 = 0 then true
  else false;;

let print_list l =
  print_string "[";
  print_int (List.nth l 0);
  for i = 1 to List.length l -1 do
    print_string " ";
    print_int(List.nth l i) done;
  print_string "]";;

let rec del_cht l =
  match l with
      []->[]
      |h::t -> if cht h then h::del_cht t
              else del_cht t;;

print_list (del_cht l);;
print_string "\n";;
