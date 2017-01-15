let rec plus n m=
	if m>=0 then
		(plus n (m-1);
		print_int n;
		print_string " + ";
		print_int m;
		print_string " = ";
		print_int (n+m);
		print_string "\n");;


let rec table n m=
	if n>=0 then
		(table (n-1) m;
		plus n m;
		print_string "\n");;


let n = read_int();;
let m = read_int();;
table n m;;
