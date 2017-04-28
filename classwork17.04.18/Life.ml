(*
"Константы"
*)
Graphics.open_graph " 700x700";;
let pintamimo = [(1,6); (2,6); (3,7); (3,5); (4,6);(5,6);(6,6);(7,6);(8,7);(8,5);(9,6);(10,6)];;
let glider = [(0, 0);(0, 1);(0, 2);(1, 2);(2, 1)];;
(*
Функции
*)
let rec draw_relation l =
	List.iter (fun(x,y) -> Graphics.draw_circle (x * 10 + 100) (y * 10 + 100) 3)  l;;

let move_r (sx, sy) l = List.map (fun (x, y) -> (x + sx, y + sy)) l;;

let (++) a b = a @ List.filter (fun w -> not (List.mem w a)) b;;

let neighbour = [(-1,0);(0,-1);(1,0);(0,1);(1,1);(-1,-1);(-1,1);(1,-1)];;

let contour l = List.flatten (List.map (fun x -> move_r x l) neighbour);;

let count el dict = List.length (List.filter (fun x -> el = x) dict);;

let rec unique l = match l with
	l1::ls -> l1::unique (List.filter (fun x -> x <> l1) ls)
	|[] -> [];;

let make_step  l =
	let cont = contour l in
	let remaining = List.filter (fun x -> count x cont = 2 ) l in
	let born = List.filter (fun x -> count x cont = 3) (cont ++ l) in unique (remaining ++ born);;

let rec repeat world n =
	if n < 0 then () else (
		Graphics.clear_graph();
		Graphics.set_color Graphics.red;
		draw_relation (world);
		let w = Graphics.read_key() in
		if w = char_of_int 27 then ()
		else repeat (make_step world) (n - 1)
	);;
(*
Тело программы
*)
repeat pintamimo 100;;
