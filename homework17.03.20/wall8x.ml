print_string "Введите начальный x : ";;
let x = read_int();;
print_string "Введите начальный y : ";;
let y = read_int();;
print_string "Введите движение по x : ";;
let dx = read_int();;
print_string "Введите движение по y : ";;
let dy = read_int();;

let x1 = 20;;
let x2 = 600;;
let x1'= 200;;
let x2'= 400;;
let y1 = 15;;
let y2 = 400;;
let y1'= 115;;
let y2'= 300;;

let r = 20;;
Graphics.open_graph " 700x700";;

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

let speed2' x dx x1 x2 r =
  if (x + 2 * dx - r < x1) then (1, abs(dx))
  else if (x + 2 * dx + r > x2) then (-1, -abs(dx))
  else (0, dx);;

let speed2 x dx dy x1 x1' x2 x2' y1 y1' y2 y2' r =
  if (fst(speed2' x dx x1 x2 r) = 0) && (fst(speed2' y dy y1 y2 r)=0) then
  (
      if ((x+ 2*dx +r < x1') && (x < x1') && (x1 < x)) || ((x+ 2*dx -r > x2') && (x > x2') && (x2 > x)) then (dx, dy)
      else
      (
          if (y +2*dy +r < y1') || (y +2*dy -r > y2') then (dx, dy)
          else
          (
              if y < y1' then (dx, -abs(dy))
              else if y > y2' then (dx, abs(dy))
              else
              (
                    if x < x1' then (-abs(dx), dy)
                    else (abs(dx), dy)
              )
          )
      )
  )
  else (snd(speed2' x dx x1 x2 r), snd(speed2' y dy y1 y2 r));;



let circle x y x' y' r =
  (Graphics.set_color(Graphics.rgb 255 255 255);
  Graphics.draw_circle x y r;
  Graphics.set_color(Graphics.rgb 0 255 0);
  Graphics.draw_circle x' y' r);;

let rec animation x y dx dy r x1 x2 y1 y2 =
  for i = 1 to 100000 do () done;
  rect x1 x2 y1 y2;
  if Graphics.key_pressed() then ()
  else (
  circle x y (x+dx) (y+dy) r;
  animation (x+dx) (y+dy) (speed x dx x1 x2 r) (speed y dy y1 y2 r) r x1 x2 y1 y2);;

let rec animation2 x y (dx, dy) r x1 x1' x2 x2' y1 y1' y2 y2' =
  for i = 1 to 100000 do () done;
  rect x1 x2 y1 y2;
  rect x1' x2' y1' y2';
  if Graphics.key_pressed() then ()
  else
  (
      circle x y (x+dx) (y+dy) r;
      animation2 (x+dx) (y+dy) (fst(speed2 x y dx dy x1 x1' x2 x2' y1 y1' y2 y2' r), snd(speed2 x y dx dy x1 x1' x2 x2' y1 y1' y2 y2' r)) r x1 x1' x2 x2' y1 y1' y2 y2'
  );;

rect x1 x2 y1 y2;;
rect x1' x2' y1' y2';;
Graphics.draw_circle x y r;;
animation2 x y (dx, dy) r x1 x1' x2 x2' y1 y1' y2 y2';;
