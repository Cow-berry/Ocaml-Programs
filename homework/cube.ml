let rec cube a =
    if a > 0 then
    begin
      cube (a-1);
      print_int (a*a*a);
      print_string "\n"
    end;;

let a = read_int();;
cube a
