let rec num a b =
  if b >= a then
    (num a (b-1);
    if b<10 then
      print_string " ";
    print_int b;
    print_string" ";);;

let rec table a b=
  if a >= 0 then
    (table (a-1) (b-1);
    num a b;
    print_string "\n");;


let a = read_int();;
table a (2*a);;
print_string "\n";;
