Graphics.open_graph " 700x700";;
Graphics.set_window_title "Kochenyuk Anatoly 8m. Circles";;
type cmd = L|D|R;;
for i = 1 to 500 do
  Graphics.draw_circle (Random.int 700) (Random.int 700) (Random.int 100) done;;

let _ = Graphics.read_key ();;
