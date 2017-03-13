let pair n m = 
  if n <> m then (m, (m+1))::[]
  else (m, 1)::[];;

let make n m =
  if m > 1 then pair n (n-m) @ make n (m-1);;

let print_str n l m =
for i = 1 to List.length m do
  if List.mem (n, i) l then print_int 1
  else print_int 0 done;;

let print_rel m l =
  for i = 1 to List.length m do
    print_str i l m ;
    print_string "\n" done;;

let print_list l =
  print_string "[";
  print_int (List.nth l 0);
  for i = 1 to List.length l -1 do
    print_string " ";
    print_string "(";
    print_int(fst(List.nth l i));
    print_string ", ";
    print_int(snd(List.nth l i));
    print_string ")" done;
  print_string "]";;

print_list(make 2 2);;
