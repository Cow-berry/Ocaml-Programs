let n = read_int ();;
let print_str n l m =
  for i = 1 to List.length m do
    if List.mem (n, i) l then print_int 1
    else print_int 0 done;;

let print_rel m l =
  for i = 1 to List.length m do
    print_str i l m ;
    print_string "\n" done;;

let del'' i j =
  (i, j)::[];;

let rec del' i n =
  if n > 1 then (del'' i n) (@) (del'' i (n-1));;

let rec del n m =
  if m > 1 then  (del' n m) @ (del' m (m-1));;

print_rel (del n n);;
