Graphics.open_graph " 700x700";;
Graphics.set_window_title "Kochenyuk Anatoly 8m. Lines";;
Graphics.moveto (Random.int 700) (Random.int 700);;
for i = 1 to 500 do
Graphics.lineto (Random.int 700) (Random.int 700) done;;
type cmd = L|D|R;;
let _ = Graphics.read_key ();;
