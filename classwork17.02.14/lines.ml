Graphics.open_graph " 700x700";;
Graphics.set_window_title "Kochenyuk Anatoly 8m. lines";;

let rec lines i y=
  Graphics.set_color (Graphics.rgb (Random.int 255) (Random.int 255) (Random.int 255));
  Graphics.moveto 0 (y);
  Graphics.lineto 700 y;
  if i > 0 then lines (i-1) (y+2);;

lines 500 0;;
let _ = Graphics.read_key ();;
