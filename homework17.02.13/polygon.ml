let p = read_int();;
Graphics.set_window_title "Kochenyuk Anatoly 8m. Polygon";;
Graphics.open_graph " 700x700";;
Graphics.moveto 100 100;;
let pi = 3.14159265;;
let n = 5;;
let l = 50;;
let polygon n l =
  for i = 1 to n do
    Graphics.lineto (abs(l*cos(2 *. pi*.(i +. 1) /. n))) (abs(l*.sin(2 *. pi*.(i +. 1) /. n))) done ;;
type cmd = L|D|R;;
let _ = Graphics.read_key ();;
