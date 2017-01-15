let rec repeat_char n str =
    if n > 1 then
    (
    repeat_char (n-1) str;
    Printf.printf  "%s" str
    );;

let rec volna n k =
	if k > 1 then volna n (k-1);
	print_string"/";
  repeat_char k "~";
  print_string "\\\\_";;

let n = read_int();;
volna n n;;
print_string "\n";;
