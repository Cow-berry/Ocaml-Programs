let rec star a =
  if a > 0 then (
  star (a-1);
  print_string "*";
  );;

let spaceorstar x y r =
    if r*r > (x - r) * (x - r) + (y - r) * (y - r) then
        print_string "*"
    else print_string " ";;

let rec line x y r =
  if x>0 then (
    line (x-1) y r;
    spaceorstar x y r;
  );;

let rec circle x y r =
    if y > 0 then (
    circle x (y-1) r;
    line x y r;
    print_string "\n";
    );;

let r = read_int();;
circle (2*r) (2*r) r;;
