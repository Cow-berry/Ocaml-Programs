Graphics.open_graph " 800x800";;
Graphics.set_window_title "Kochenyuk Anatoly 8m. Sweeden flag";;
Graphics.set_color (Graphics.rgb 255 0 0);;
Graphics.fill_rect 0 0 800 800;;
Graphics.set_color (Graphics.rgb 255 255 255);;
Graphics.fill_rect (800/5) (2*800/5) (3*800/5) (800/5);;
Graphics.fill_rect (2*800/5) (800/5) (800/5) (3*800/5);;
let _ = Graphics.read_key ();;
