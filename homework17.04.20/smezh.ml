(*
создание матрицы смежности
*)
let rec gen1 x0 a b r=
  if a<=b then [List.mem (x0,a) r]@ gen1 x0 (a+1) b r
  else [];;


let rec gen2 x1 x2 y1 y2 r=
  if x1<=x2 then gen1 x1 y1 y2 r :: gen2 (x1+1) x2 y1 y2 r
  else [];;


(*воссоздание списка координат*)
let rec restore1 x y l =
match l with
  [] -> []
  | h::t -> if h then [(x, y)] @ restore1 x (y+1) t
            else restore1 x (y+1) t;;

let rec restore x matrix =
  match matrix with
  [] -> []
  | h::t -> restore1 x 0 h :: restore (x+1) t;;



let rec print_line s i =
  if i > 0 then
  (
    print_string s;
    print_line s (i-1);
  )
  else print_string"\n";;

let draw_row l =
  List.iter (fun x -> if x then Printf.printf "o|" else Printf.printf " |") l;
  print_string "\n";;

let draw_bool matrix =
  List.iter (fun y -> draw_row y) matrix;;

let glider = [(0,0);(0,1);(0,2);(1,2);(2,1)];;
let pintamimo = [(1,6); (2,6); (3,7); (3,5); (4,6);(5,6);(6,6);(7,6);(8,7);(8,5);(9,6);(10,6)];;
let matrix = gen2 1 10 5 7 pintamimo;;(*в аргументах -- минимальные и максимальные значения по x и y*)
draw_bool matrix;;
