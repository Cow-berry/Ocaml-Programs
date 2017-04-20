Graphics.open_graph " 700x700";;
let filt (x, y) =
let i =x/2 in
let j = y/2 in
 i=0 || i=4 || j=0 || j=4 || (i<>2 && j<>2);;

let plus n r = List.filter filt r;;

let rec gen_full n a b=
  if b < n then
    if a < n then ((a), (b)):: gen_full n (a+1) b
    else gen_full n 0 (b+1)
  else [];;
let n = 10;;
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
  Graphics.set_color(Graphics.rgb 255 0 0);
  match bin with
  [] -> ()
  |h::t -> Graphics.draw_circle (l * (fst(h)+1) + l / 2 ) (700 - (l * (snd(h) + 1) + l / 2)) r;
          draw_circles r l t;;

let draw n r l bin =
  draw_table n l;
  draw_circles r l bin;;

let r' = gen n;;
let r = plus n r';;
draw n 2 8 r;;
while not( Graphics.key_pressed()) do
  ()
done;;
