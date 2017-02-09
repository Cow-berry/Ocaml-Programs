(*дз
(1) Прибавить к каждому элементу списка по 1
(2) [101;2;17] -> [(0, 101); (1, 2); (2, 17)]
numbers 'a list -> (int * 'a) list
(3) nth взять n-ый элемент списка
(4) zip : 'a list -> 'b list -> 'a и 'b
unzip :
(5) bool list - пр дв
 *)
let max a b =
  if a > b then a
  else b;;

(* 3 *)
 let rec nth l i =
  match l with
   | [] -> 0
   | h::t ->
         if i = 0 then h
         else nth t (i-1);;
(* просто печать *)
let rec print_list = function
[] -> ()
| h::t -> print_int h ; print_string " " ; print_list t;;
(* 1 *)
let rec inc  l =
  match l with
    [] -> []
    |h::t -> (h+1)::(inc t);;
(* 2 *)
let rec numbers l =
  match l with
    [] -> print_string ""
    |h::t ->
      for i = 0 to List.length (l)-1 do
        Printf.printf "(%d, %d)" i (nth l i) done ;;

(* 4 *)
let zip l1 l2 =
  for i = 0 to (max (List.length l1) (List.length l2) )-1 do
    Printf.printf "(%d, %d)" (nth l1 i) (nth l2 i) done;;

(* Проверка *)
let l = [1;3;5;7;8;0;2];;
let l2 = [2;4;6;9;1;3;1;2;3];;
print_string "l = [";;
print_list l;;
print_string "]\nl2 = [";;
print_list l2;;
print_string "]\ninc l = [";;
print_list (inc l);;
print_string "]\nnumbers l = [";;
numbers l;;
print_string "]\nzip l l2 = [";;
zip l l2;;
print_string "]\n";;
