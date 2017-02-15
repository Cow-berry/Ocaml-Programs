Graphics.open_graph " 700x700";;
Graphics.set_window_title "Kochenyuk Anatoly 8m. 500 randlines";;
let col = [90; 0; 157;  0; 0; 255 ; 0; 191; 255; 0; 255; 0; 255; 255; 0; 255; 128; 0; 255; 0; 0]
let rec nth l i =
	match l with
	[] -> failwith "Empty"
	|h::t ->
		if i = 0 then h
		else nth t (i-1);;

Graphics.set_line_width 10;;

let rec rainbow i x y=
  Graphics.set_color (Graphics.rgb (nth col (i*3-3)) (nth col (i*3-2)) (nth col (i*3-1)));
  Graphics.draw_arc 350 100 (x+30) (x+30) 0 179;
  if i < 7 then rainbow (i+1) (x+30) (x+30);;

rainbow 1 30 30;;
let _ = Graphics.read_key ();;
