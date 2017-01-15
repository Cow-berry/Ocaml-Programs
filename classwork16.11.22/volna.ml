let rec repeat_char n str =
    if n > 1 then
    (
    repeat_char (n-1) str;
    Printf.printf  "%s" str
    );;

let greben n =
	print_string"/";
	repeat_char (n+1) "~";
	print_string "\\_";; 

let rec volna n k =
	if k > 1 then volna n (k-1);
	greben n ;;
	

let n = read_int();;
volna n 10;;