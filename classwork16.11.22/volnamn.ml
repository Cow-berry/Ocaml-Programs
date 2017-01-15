let rec repeat_char n str =
    if n > 1 then
    (
    repeat_char (n-1) str;
    Printf.printf  "%s" str
    );;

let greben n m =
	print_string"/";
	repeat_char (n+1) "~";
	print_string "\\";
	repeat_char (m+1) "_";;
	

let rec volna n m k =
	if k > 1 then volna n m (k-1);
	greben n m;;
	

let n = read_int();;
let m = read_int();;
volna n m 10;;