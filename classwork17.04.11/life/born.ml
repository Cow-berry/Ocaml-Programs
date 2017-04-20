Graphics.open_graph " 700x700";;
let rec gen_full n a b=
  if b < n then
    if a < n then ((a), (b)):: gen_full n (a+1) b
    else gen_full n 0 (b+1)
  else [];;

let gen n = gen_full n 0 0;;

let rec neighbor' (x, y) r i j=
  if i < 2 then
    if j < 2 then
      if (x, y) <> (x+i, y+j) && List.mem (x+i, y+j) r then
        1 + neighbor' (x, y) r (j+1) i
      else
        neighbor' (x, y) r (j+1) i
    else neighbor' (x, y) r (-1) (i+1)
  else
    0;;

let neighbor (x, y) r =
  neighbor' (x, y) r (-1) (-1);;

let born' r gen =
  List.filter (fun(x, y) -> (not(List.mem (x, y)r)) && (neighbor (x, y) r = 3)) gen;;

let born r n =
  born' r (gen n);;

let death' r gen =
  List.filter (fun(x, y) -> (not(List.mem (x, y)r)) && (neighbor (x, y) r < 2 || neighbor (x, y) r >3))gen;;

let death r n =
  death' r (gen n);;

let const' r gen =
  List.filter (fun(x, y) -> (List.mem (x, y) r) && (neighbor (x, y) r = 2)) gen;;

let const r n =
  const' r (gen n);;

let add_unique r1 r2 = r1 @ (List.filter(fun x -> not (List.mem x r1))r2);;
let comb a b = add_unique a b;;



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

let rec draw_circles r l bin a b c=
  Graphics.set_color(Graphics.rgb a b c);
  match bin with
  [] -> ()
  |h::t -> Graphics.draw_circle (l * (fst(h)+1) + l / 2 ) (700 - (l * (snd(h) + 1) + l / 2)) r;
          draw_circles r l t a b c;;

let draw n r l bin =
  draw_table n l;
  draw_circles r l bin 255 0 0;;
let n = 10;;


(*
n -- сторона квадратного поля
с -- изначальное бинарное отношение
r -- радиус круга, обозначающего наличие пары
l -- сторона маленткого квадратика
*)

let rec main n c r l =
  draw n r l c;
  for i = 1 to 40000000 do
    ();
  done;
  draw_circles r l c 255 255 255;
  let c_new = comb (const c n)(born c n) in
  main n c_new r l;;

let r = [(2, 1);(2, 2);(2, 3)];;

main 10 r 2 8;;
