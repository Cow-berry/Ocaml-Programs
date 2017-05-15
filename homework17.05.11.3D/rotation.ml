Graphics.open_graph " 800x800";;
Graphics.set_window_title "Kochenyuk Anatoly 8m. cube";;
Graphics.set_line_width 2;;
(* cube *)
(* let points = [[1.; 1.; 1.];[1.; 1.; -.1.];[1.; -.1.; 1.];[1.; -.1.; -1.];
            [-.1.; 1.; 1.];[-.1.; 1.; -.1.];[-.1.; -.1.; 1.];[-.1.; -.1.; -.1.]];;

let edges = [(0, 1);(0, 2);(1, 3);(2, 3);
            (5, 4);(4, 6);(6, 7);(7, 5);
            (5, 1);(4, 0);(6, 2);(7, 3)];; *)
(* dodeq *)
let points = [
[0.469;0.469;0.469];
[0.290;0.000;0.759];
[-0.759;-0.290;0.000];
[0.759;0.290;0.000];
[-0.469;0.469;-0.469];
[0.000;-0.759;-0.290];
[-0.759;0.290;0.000];
[0.469;-0.469;0.469];
[-0.469;0.469;0.469];
[-0.469;-0.469;0.469];
[0.469;-0.469;-0.469];
[0.290;0.000;-0.759];
[-0.469;-0.469;-0.469];
[0.000;-0.759;0.290];
[0.000;0.759;-0.290];
[-0.290;0.000;0.759];
[0.759;-0.290;0.000];
[-0.290;0.000;-0.759];
[0.469;0.469;-0.469];
[0.000;0.759;0.290];
];;

let edges = [
  (9,13);(13,7);(7,1);(1,15);(15,9);
  (6,4);(4,14);(14,19);(19,8);(8,6);
  (12,5);(5,13);(13,9);(9,2);(2,12);
  (6,2);(2,12);(12,17);(17,4);(4,6);
  (16,10);(10,11);(11,18);(18,3);(3,16);
  (19,8);(8,15);(15,1);(1,0);(0,19);
  (16,7);(7,1);(1,0);(0,3);(3,16);
  (5,12);(12,17);(17,11);(11,10);(10,5);
  (18,14);(14,4);(4,17);(17,11);(11,18);
  (16,10);(10,5);(5,13);(13,7);(7,16);
  (2,6);(6,8);(8,15);(15,9);(9,2);
  (19,0);(0,3);(3,18);(18,14);(14,19);
];;

let degInRad r = r/.180.*. 3.141592653589793238462643;;

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

let rec main points edges a b c scl=
  Graphics.clear_graph();
  draw (scale scl (rotate a b c points)) edges;
  if Graphics.key_pressed() then (
    let k = Graphics.read_key() in
    if k = 'a' then (
      main points edges (a-.3.) b c scl
    )
    else if k = 'd' then (
      main points edges (a+.3.) b c scl
    )
    else if k = 'w' then (
      main points edges a (b+.3.) c scl
    )
    else if k = 's' then (
      main points edges a (b-.3.) c scl
    )
    else if k = 'q' then (
      main points edges a b (c-.3.) scl
    )
    else if k = 'e' then (
      main points edges a b (c+.3.)  scl
    )
    else if k = 'z' then (
      main points edges a b c (scl + 10)
    )
    else if k = 'x' then (
      main points edges a b c (scl - 10)
    )
  )
  else(
    for i = 1 to 1000000 do () done;
    main points edges a b c scl
  )
;;

main points edges 0. 0. 0. 100;;
