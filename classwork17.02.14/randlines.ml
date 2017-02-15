Graphics.open_graph " 700x700";;
Graphics.set_window_title "Kochenyuk Anatoly 8m. 500 randlines";;
for i = 1 to 50000 do
  Graphics.moveto (Random.int 700) (Random.int 700);
  Graphics.lineto (Random.int 700) (Random.int 700);
  Graphics.set_color (Graphics.rgb (Random.int 255) (Random.int 255) (Random.int 255)) done;;
let _ = Graphics.read_key ();;
