Graphics.open_graph " 700x700";;
let n = 10;;
let bin = [(1, 0);(1, 1);(2, 4);(4, 5);(9, 3)];;

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

let rec draw_circles r bin =
  match bin with
  [] -> ()
  |h::t -> Graphics.draw_circle (8 * (fst(h)+1) -2*r) (700 - (8 * (snd(h) + 1))-2*r) r;
          draw_circles r t;;


draw_table n 8;;
draw_circles 2 bin;;

while true do
  ()
done;;
