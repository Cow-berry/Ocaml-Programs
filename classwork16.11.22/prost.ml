let rec prime_rec k e n =
		if n < 2 then false
		else if k>e then true
		else if n mod k = 0 then false
		else prime_rec (k+1) e n;;

let prime n =
		prime_rec 2 (n/2) n;;

let rec all_primes n =
		if n > 1 then all_primes (n-1);
		if prime n then Printf.printf "%d, " n;;

let rec divisions_counter_rec k e n c =
		if n < 2 then 0
		else if k>e then 0
		else if n mod k = 0 then divisions_counter_rec (k+1) e n (c+1)
		else divisions_counter_rec (k+1) e n c;;

let divisions_counter n=
		divisions_counter_rec 2 (n/2) n 0;;


let n = read_int();;
if prime n then print_string "prime\n"
else print_string "not prime\n";;
print_string "Все простые до n : ";;
all_primes n;;
print_string"\nВсе простые до n в квадрате : ";;
all_primes (n*n);;
print_string"\n";;
print_int (divisions_counter n );;
