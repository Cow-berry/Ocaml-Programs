let rec multi x k =
  if k>0 then
    (multi x (k-1);
    if (x*k)<10 then
      print_string " ";
    if (x*k)<100 then
      print_string " ";
    if (x*k)<1000 then
      print_string " ";
    print_int (x*k);
    print_string " ");;

let rec table a b =
  if a>0 then
    (table (a-1) b;
    multi a b;
    print_string "\n");;


let a = read_int();;
table a a;;
print_string "\n";;
