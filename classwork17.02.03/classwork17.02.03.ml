(*

*)
let rec reverse a =
  match a with
    [] -> []
    |h::t -> reverse t @ [h];;
(*смотрит голову и хвост второй элемент аккамулятор *)
let rec rev l =
  let rec rev' l a =
    match l with
      [] -> a
      |h::t -> rev' t (h::a) in
    rev' l [];;

let rec gen n = if n>0 then n::gen (n-1) else [];;
let g = gen 1000;;
let rev g = reverse g then print_string"!!!";;
List.iter (Printf.printf "%d :") (rev g);;
print_int (List.length(rev (gen 250000)));;
(*Ковариантно с вами
Контрвариантно против вас
любая функция ковариантна по аргументам и контрвариантна по значению

f: int list -> int list

let f x = length x::x
f(f[a])=[2; 1; 1]
f[1] = [1; 1]
f(f[1])=[2; 1; 1]

f:int list -> int list -> int llist

let f a =
  f(length a::a)

let g a = h (length a::a)
let h a = a;;

let f x =
  let t = g x in
    length x :: x;;

[]
f [] = [1; 0];;
----------------------------------
let inc l =
  match l with
    [] -> []
    |h::t -> h+1::inc t;;
----------------------------------
let inc2 l a = match l with
  [] -> a
  |h::t -> inc t ((h+1)::t);;
----------------------------------
inc [1; 2] [] => inc [2] [2] => inc [] [3; 2] =[3; 2]
[1; 2] => 2:: inc [2] => 2::3::[]
-------------------------------------
let f x a =
  match x eith
    x1::xs -> f xs (x1::a)
    |[] -> a;;
Список --- упорядоченная коллекция произвольного числа элементов одного типа.

дз
(1) Прибавить к каждому элементу списка по 1
(2) [101;2;17] -> [(0, 10); (1, 2); (2, 17)]
numbers 'a list -> (int * 'a) list
(3) nth взять n-ый элемент списка
(4) zip : 'a list -> 'b list -> 'a и 'b
unzip :
(5) bool list - пр дв
 *)
