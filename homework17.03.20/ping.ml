Graphics.open_graph " 700x700";;
let x1 = 10;;
let x2 = 600;;
let y1 = 10;;
let y2 = 410;;
let dx = 3;;
let dy = 5;;
let ping_x1 = 500;;
let ping_x2 = 520;;
let ping_y1 = 160;;
let ping_y2 = 240;;
let ping_dv = 5;;
let ball_x = 20;;
let ball_y = 20;;
let r = 5;;

let rect x1 x2 y1 y2 =
  Graphics.moveto x1 y1;
  Graphics.lineto x1 y2;
  Graphics.lineto x2 y2;
  Graphics.lineto x2 y1;
  Graphics.lineto x1 y1;;

let draw_base ping_x1 ping_x2 ping_y1 ping_y2=
rect x1 x2 y1 y2;
Graphics.set_color(Graphics.black);
rect ping_x1 ping_x2 ping_y1 ping_y2;;


let speed_y x dx x1 x2 r =
  if (x + 2 * dx - r < x1) then abs(dx)
  else if (x + 2 * dx + r > x2) then -abs(dx)
  else dx;;

let speed_x y dy y1 y2 r =
  if (y + 2*dy -r < y1) then abs(dy)
  else if (y + 2*dy +r > y2) then 0
  else dy;;

let speed x y dx dy r ping_x1 ping_x2 ping_y1 ping_y2 =
  if (x + 2*dx +r > ping_x1) && (y + 2*dy +r > ping_y1) && (y + 2*dy -r < ping_y2) then (-dx, dy)
  else (speed_x x dx x1 x2 r , speed_y y dy y1 y2 r);;


let circle x y x' y' r =
  Graphics.set_color(Graphics.rgb 255 255 255);
  Graphics.draw_circle x y r;
  Graphics.set_color(Graphics.rgb 0 255 0);
  Graphics.draw_circle x' y' r;;
(*
let ping ping_y1 ping_y2 ping_dv k =
  match k with
    'w' -> (ping_y1 + ) *)

let rec animation x1 x2 y1 y2 ball_x ball_y dx dy ping_x1 ping_x2 ping_y1 ping_y2 ping_dv r =
  for i = 1 to 1000000 do () done;
  if speed_y ball_x dx x1 x2 r = 0 then
  (
      Graphics.background = Graphics.red;
      Graphics.clear_graph();
  )
  else
  (
    Graphics.set_color(Graphics.white);
    rect ping_x1 ping_x2 ping_y1 ping_y2;
    circle ball_x ball_y (ball_x+dx) (ball_y+dy) r;
    Graphics.set_color(Graphics.black);
    if Graphics.key_pressed() then (
      let k = Graphics.read_key() in
      if k = 'w' && (ping_y2 + ping_dv) < y2 then (
        let ping_y1_new = ping_y1 + ping_dv in
        let ping_y2_new = ping_y2 + ping_dv in

        draw_base ping_x1 ping_x2 ping_y1_new ping_y2_new;
        animation x1 x2 y1 y2 (ball_x+dx) (ball_y+dy) (fst(speed ball_x ball_y dx dy r ping_x1 ping_x2 ping_y1 ping_y2)) (snd(speed ball_x ball_y dx dy r ping_x1 ping_x2 ping_y1 ping_y2)) ping_x1 ping_x2 ping_y1_new ping_y2_new ping_dv r
      ) else if k = 's' && (ping_y1 - ping_dv) > y1 then (
        let ping_y1_new = ping_y1 - ping_dv in
        let ping_y2_new = ping_y2 - ping_dv in
        draw_base ping_x1 ping_x2 ping_y1_new ping_y2_new;
        animation x1 x2 y1 y2 (ball_x+dx) (ball_y+dy) (fst(speed ball_x ball_y dx dy r ping_x1 ping_x2 ping_y1 ping_y2)) (snd(speed ball_x ball_y dx dy r ping_x1 ping_x2 ping_y1 ping_y2)) ping_x1 ping_x2 ping_y1_new ping_y2_new ping_dv r
      )else
      (
        draw_base ping_x1 ping_x2 ping_y1 ping_y2;
        animation x1 x2 y1 y2 (ball_x+dx) (ball_y+dy) (fst(speed ball_x ball_y dx dy r ping_x1 ping_x2 ping_y1 ping_y2)) (snd(speed ball_x ball_y dx dy r ping_x1 ping_x2 ping_y1 ping_y2)) ping_x1 ping_x2 ping_y1 ping_y2 ping_dv r

      )
      )
    else (
        draw_base ping_x1 ping_x2 ping_y1 ping_y2;
        animation x1 x2 y1 y2 (ball_x+dx) (ball_y+dy) (fst(speed ball_x ball_y dx dy r ping_x1 ping_x2 ping_y1 ping_y2)) (snd(speed ball_x ball_y dx dy r ping_x1 ping_x2 ping_y1 ping_y2)) ping_x1 ping_x2 ping_y1 ping_y2 ping_dv r
      );

    (* ); *)
   );;

animation x1 x2 y1 y2 ball_x ball_y dx dy ping_x1 ping_x2 ping_y1 ping_y2 ping_dv r;;
