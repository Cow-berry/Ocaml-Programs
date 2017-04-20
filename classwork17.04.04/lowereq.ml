Graphics.open_graph " 700x700";; 
     
let n = 70;;
let r = 1;;
let rec gen_full n a b=
  if b < n then
    if a < n then ((a), (b)):: gen_full n (a+1) b
    else gen_full n 0 (b+1)
  else [];;

let gen n = gen_full n 0 0;;

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

let bi = gen n;;

let bin = List.filter (fun (x, y) -> x<=y) bi ;;

draw_table n 8;;
draw_circles r 8 bin;;
while true do
  ()
done;;
