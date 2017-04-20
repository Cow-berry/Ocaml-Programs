let rec neighbor (x, y) r i j=
  if i < 2 then
    if j < 2 then
      if (x, y) <> (x+i, y+j) && List.mem (x+i, y+j) r then
        1 + neighbor (x, y) r (j+1) i
      else
        neighbor (x, y) r (j+1) i
    else neighbor (x, y) r (-1) (i+1)
  else
    0;;

let print_list l =
  List.map(fun(x, y) -> Printf.printf "(%d, %d)\n" x y) l;;

let r = [(0, 0);(1, 0);(1, 1)];;
let n = neighbor (1, 1) r (-1) (-1);;
print_int n;;
