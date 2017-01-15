let min a b =
	if a < b then
		a
	else b;;

let rec line n =
	if n > 0 then (
		line (n-1);
		print_string "1 "
	);;

let rec rect n  m =
	if m > 0 then (
		line (min m n);
		print_string "\n";
		rect n (m - n);
	);;

let n = read_int();;
let m = read_int();;
rect n m;;
