Graphics.open_graph " 700x700";;
let r' = [(1, 1);(1, 2);(3, 3)];;

let rec neighbor r =
  match r with
    []->[]
    |h::t ->  @ (neighbor t);;

let r = neighbor r';;

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

draw 6 2 8 r;;

while not( Graphics.key_pressed()) do
  ()
done;;
