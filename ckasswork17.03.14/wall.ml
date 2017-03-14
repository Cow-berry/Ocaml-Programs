print_string "Введите начальный x : ";;
let x = read_int();;
print_string "Введите начальный y : ";;
let y = read_int();;
print_string "Введите движение по x : ";;
let dx = read_int();;
print_string "Введите движение по y : ";;
let dy = read_int();;
print_string "Введите координату вертикальной стены : ";;
let x0 = read_int();;
Graphics.open_graph " 700x700";;
Graphics.moveto x0 0;;
Graphics.lineto x0 700;;

let rec exit x y dx dy r i =
Graphics.draw_circle x y r;
Graphics.set_color(Graphics.rgb 255 255 255);
for i = 1 to 1000000 do () done;
Graphics.draw_circle x y r;
Graphics.set_color(Graphics.rgb 0 255 0);
Graphics.moveto x0 0;
Graphics.lineto x0 700;
let x1 = x+dx in
let y1 = y+dy in
if i > 0 then exit x1 y1 dx dy r (i-1);
 ;;

let rec animation x y dx dy x0 r =
  if Graphics.key_pressed() then ()
  else (
    Graphics.set_color(Graphics.rgb 255 255 255);
    Graphics.draw_circle x y r;
    Graphics.set_color(Graphics.rgb 0 255 0);
    Graphics.moveto 500 0;
    Graphics.lineto 500 700;
    if (x+dx>x0-r) then (
      let x1 = 2*(x0-r)-x-dx in
      let y1 = y+dy in
      let dx1 = -dx in
      let dy1 = dy in
      exit x1 y1 dx1 dy1 r 100;

      for i = 1 to 1000000 do () done;
      animation (x1+100*dx1) (y1+100*dy1) dx1 dy1 x0 r
    ) else (
      let x1 = x+dx in
      let y1 = y+dy in
      let dx1 = dx in
      let dy1 = dy in
      Graphics.draw_circle x1 y1 r;
      for i = 1 to 1000000 do () done;
      animation x1 y1 dx1 dy1 x0 r));;


animation x y dx dy x0 20;;
