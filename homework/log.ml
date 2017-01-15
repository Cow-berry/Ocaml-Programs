let rec log n =
    if (n / 2) < 1 then 0
    else log (n/2) + 1;;

(*let rec test a =
      if a > 0 then
      begin
        test (a-1);
        print_int (a);
        print_string " ";
        print_int (log a);
        print_string "\n"
      end;;

test 100;;*)
let a = read_int();;
print_int(log a);;
