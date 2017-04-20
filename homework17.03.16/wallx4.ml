print_string "Введите начальный x : ";;
let x = read_int();;
print_string "Введите начальный y : ";;
let y = read_int();;
print_string "Введите движение по x : ";;
let dx = read_int();;
print_string "Введите движение по y : ";;
let dy = read_int();;
let x1 = 0;;
let x2 = 1300;;
let y1 = 0;;
let y2 = 800;;
let r = 4;;
Graphics.open_graph " 1400x700";;

let rect x1 x2 y1 y2 =
  Graphics.moveto x1 y1;
  Graphics.lineto x1 y2;
  Graphics.lineto x2 y2;
  Graphics.lineto x2 y1;
  Graphics.lineto x1 y1;;


let speed x dx x1 x2 r =
  if (x + 2 * dx - r < x1) then abs(dx)
  else if (x + 2 * dx + r > x2) then -abs(dx)
  else dx;;



let circle x y x' y' r =
  (*Graphics.set_color(Graphics.rgb 255 255 255);
  Graphics.draw_circle x y r;*)
  Graphics.set_color(Graphics.rgb (Random.int 255)(Random.int 255)(Random.int 255) );
  Graphics.draw_circle x' y' r;;

let rec animation2 x y dx dy r x1 x2 y1 y2 =

(*  for i = 1 to 100 do () done;*)
  rect x1 x2 y1 y2;
  if Graphics.key_pressed() then ()
  else (
  circle x y (x+dx) (y+dy) r;
  animation2 (x+dx) (y+dy) (speed x dx x1 x2 r) (speed y dy y1 y2 r) r x1 x2 y1 y2);;

rect x1 x2 y1 y2;;
Graphics.draw_circle x y r;;
animation2 x y dx dy r x1 x2 y1 y2;;
