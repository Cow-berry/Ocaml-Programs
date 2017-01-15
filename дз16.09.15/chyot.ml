let even x = if x mod 2 = 0 then
    begin
      print_int x;
      print_string "\n"
    end
    ;;

let a = read_int();;
let b = read_int();;
let c = read_int();;

print_string "Result:\n";
even a;
even b;
even c;
