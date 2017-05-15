Graphics.open_graph " 800x800";;
Graphics.set_window_title "Kochenyuk Anatoly 8m. cube";;
Graphics.set_line_width 1;;

let points = [[1.; 1.; 1.];[1.; 1.; -.1.];[1.; -.1.; 1.];[1.; -.1.; -1.];
            [-.1.; 1.; 1.];[-.1.; 1.; -.1.];[-.1.; -.1.; 1.];[-.1.; -.1.; -.1.]];;

let edges = [(0, 1);(0, 2);(1, 3);(2, 3);
            (5, 4);(4, 6);(6, 7);(7, 5);
            (5, 1);(4, 0);(6, 2);(7, 3)];;

let degInRad r = r/.180.*. 3.141592653589793238462643;;
let center = (400, 400);;
let range x =
  if x >= 0. then int_of_float(ceil x)
  else int_of_float(floor x);;

let scale s f =
  List.map (fun [x; y; z] ->
              [range(float_of_int(s) *. x); range(float_of_int(s) *. y); range(float_of_int(s) *. z)]
            ) f;;

let xm ga =
  let a = degInRad ga in
  [[1.; 0.   ; 0.    ];
  [ 0.; cos a; -.sin a];
  [ 0.; sin a; cos a]];;

let ym ga =
  let a = degInRad ga in
  [[cos a; 0.; sin a];
  [ 0.    ; 1.; 0.  ];
  [-.sin a; 0.; cos a]];;

let zm ga =
  let a = degInRad ga in
  [[cos a; -.sin a; 0.];
  [ sin a; cos a; 0.];
  [ 0.   ;0.    ;1.]];;

let multi m v =
  List.map (fun w ->
    List.fold_left (+.) 0. (List.map2 ( *.) w v)) m;;

let rotate_x a fig =
  List.map (fun x -> multi (xm a) x) fig;;

let rotate_y a fig =
  List.map (fun x -> multi (ym a) x) fig;;

let rotate_z a fig =
  List.map (fun x -> multi (zm a) x) fig;;

let rotate a b c fig =
  rotate_x a (rotate_y b (rotate_z c fig));;

let line p q =
  let [x1; y1; z1] = p in
  let [x2; y2; z2] = q in
  Graphics.moveto (x1 + 400) (y1 + 400);
  Graphics.lineto (x2 + 400) (y2 + 400);;

let draw points edges =
  List.iter (fun (p, q) -> line (List.nth points p) (List.nth points q)) edges;;

draw (scale 100 (rotate 30. 30. 30. points)) edges;;
while true do
()
done;;
(*Тестировать можно в интерпритаторе*)
(* ДЗ:
1)куб в 3д
2)вращающийся куб
3)Додекаедр
4)Футбольный мячик*)
