let min a b =
	if a > b then b
	else a;;

let max a b =
	if a > b then a
	else b ;;
(* 0 *)
let rec check l e =
	match l with
		[]-> false
		|h::t ->
			if h = e then true
			else check t e;;

let rec nth l i =
	match l with
	[] -> failwith "Empty"
	|h::t ->
		if i = 0 then h
		else nth t (i-1);;

let print_list l =
	Printf.printf "[%d" (nth l 0);
	for i = 1  to List.length l-1 do
		Printf.printf ", %d"(nth l i) done;
	print_string "]\n";;

let l = [2;6;3;5;3;3;6;3;8;3;8];;
let l2 =[2;6;3;5;3;3;6;3;8;8;3];;
print_string "l = ";;
print_list l;;
print_string "l2 = ";;
print_list l2;;
let e = read_int();;
Printf.printf "Элемент %d содержится в списке --- %b\n" e (check l e);;
(* 1 *)
let ls = List.sort compare l;;
let ls2 = List.sort compare l2;;
print_string "sort (l) = ";;
print_list ls;;
print_string "sort (l2) = ";;
print_list ls2;;
(* 4 *)
let rec rm ls =
 match ls with
		[] -> []
		|h::[] -> ls
		|h1::h2::t ->
			if h1 = h2 then rm (h2::t)
			else h1::rm(h2::t);;

print_string "rm (ls) = ";;
print_list (rm ls);;
print_string "rm (ls2) = ";;
print_list (rm ls2);;
(* 6 *)
let is_equival ls1 ls2=
	if ls1 = ls2 then true
	else false;;

Printf.printf"ls = ls2 --- %b" (is_equival ls ls2);;
print_string"\n"
(* 2 *)
let rec two l =
	if l = [] then print_string"Empty"
	else for i = 0 to List.length l - 1 do
		if i mod 2 = 0 then Printf.printf "%d " (nth l i) done;;
print_string "two (l) = ";;
two l;;
print_string"\n";;
(* 5 *)
let rec from_two l =
	if l = [] then ()
	else if List.length l = 1 then ()
	else
		Printf.printf "%d" (nth l 1);
		for i = 2 to List.length l - 1 do
			Printf.printf ", %d" (nth l i) done;;

print_string "from_two (l) = [ ";;
from_two l;;
print_string " ]\n";;
(* 7 *)
let finElem l =
	if l = [] then failwith "Empty"
	else nth l (List.length l - 1);;
Printf.printf "Последний элемент l --- %d\n" (finElem l2);;
