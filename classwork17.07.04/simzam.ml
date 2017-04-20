Graphics.open_graph " 700x700";;
let add_unique r1 r2 = r2 @ (List.filter(fun x -> not (List.mem x r2))r1);;
let sim' r = List.map(fun(x, y) -> (y, x)) r;;
let sim r = add_unique (sim' r) r;;
let r = [(0, 0);(1, 2);(2,3);(1,5)];;
let r_sim = sim r;;
let print_list r = List.iter(fun (x, y) -> Printf.printf "(%d, %d)" x y) r;;
print_list r;;
let n = 6;;

let draw_table n l =
  Graphics.draw_rect l (700 -l*(n+1)) (n*l) (n*l);
  for i = 1 to n do
    if i mod 5 = 0 then
      Graphics.set_color (Graphics.rgb 0 0 0)
    else
      Graphics.set_color (Graphics.rgb 100 100 100);
    Graphics.moveto (l*(i+1)) (700 - l);
    Graphics.lineto (l*(i+1)) (700 - (n+1)*l);

    Graphics.moveto l (700 - l*(i+1));
    Graphics.lineto (l*(n+1)) (700 - l*(i+1))
    done;;

let rec draw_circles r l bin =
  Graphics.set_color(Graphics.rgb 0 0 0);
  match bin with
  [] -> ()
  |h::t -> Graphics.draw_circle (l * (fst(h)+1) + l / 2 ) (700 - (l * (snd(h) + 1) + l / 2)) r;
          draw_circles r l t;;

let draw n r l bin =
  draw_table n l;
  draw_circles r l bin;;

draw 6 2 8 r_sim;;

while not( Graphics.key_pressed()) do
  ()
done;;
