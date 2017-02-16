Graphics.open_graph " 800x800";;
Graphics.set_window_title "Kochenyuk Anatoly 8m. squares";;

for i = 1 to 200 do
  Graphics.draw_rect (i*2) (i*2) (800-4*i) (800-4*i) done;;

let _ = Graphics.read_key ();;
