Graphics.open_graph " 700x700";;

let draw_relation l =
  List.iter (fun (x, y) -> Graphics.draw_circle (x*10+100) (y*10+100) 3) l;;

let glider = [(0, 0);(1, 0);(2, 0);(2, 1);(1, 2)];;

let move_relation (sx, sy) l = List.map  (fun(x, y) -> (x+sx, x+sy)) l;;
let (++) a b= a @ List.filter (fun (x, y) -> not (List.mem (x, y) a)) b;;

let neighbours = [(1, -1);(1, 0);(1, 1);(0, -1);(0, 1);(-1, -1);(-1, 0);(-1, 1)];;

let contour l = List.flatten (List.map (fun x -> move_relation x l) neighbours);;

let count el dict = List.length (List.filter (fun x -> el = x) dict);;

let rec unique l =
  match l with
    [] -> []
    |h::t -> h::unique (List.filter (fun x -> x <> h) t);;

let make_step l =
  let cont = unique(contour l) in
  let remaining = List.filter (fun x -> count x cont = 2 ) l in
  let born = List.filter (fun x -> count x cont = 3) (unique(cont @ l)) in
  unique(remaining @ born);;

let rec repeat world n =
  if n < 0 then ()
  else (
    for i = 1 to 1000000 do
    (); done;
    Graphics.clear_graph();
    Graphics.set_color Graphics.blue;
    draw_relation (unique(contour(world)));
    Graphics.set_color Graphics.green;
    draw_relation (make_step world);
    Graphics.set_color Graphics.black;
    draw_relation world;
    let w = Graphics.read_key() in
    if w = char_of_int 27 then ()
    else repeat (make_step world) (n-1)
    );;
let r = [(0, 0);(0, 1);(0, 2)];;
repeat glider 100;;
