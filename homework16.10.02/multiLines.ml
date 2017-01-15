let rec multi x k =
  if k>0 then
    (multi x (k-1);
    Printf.printf "%4d|" (x*k););;

let rec spaces a =
  if a>0 then (
  print_string "----+";
  spaces (a-1);
  );;

let line a =
    print_string "+";
    spaces a;
    print_string "\n";;

let numbers a b =
    print_string "|";
    multi a b;
    print_string "\n";;


let rec table a b =
  if a > 0 then (
    table (a-1) b;
    numbers a b;
    line b;
    );;

let multitable a =
  line a;
  table a a;;
  print_string "\n";;

let a = read_int();;
multitable a;;
