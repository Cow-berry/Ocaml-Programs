let rec step n m =
    if n <= (m*m) then
          step n (m-1)
    else (m+1);;

let min a b =
  	if a < b then a
  	else b;;

let rec line n =
  	if n > 0 then (
    		line (n-1);
    		print_string "1 "
  	);;

let rec square n  m =
  	if m > 0 then (
    		line (min m n);
    		print_string "\n";
    		square n (m - n);
  	);;


let m = read_int();;
square (step m m) m;;
