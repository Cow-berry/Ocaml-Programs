(*type list =
  Cons of `a*`a list
  |Nil
*)
(*
Примеры :
  Nil                   []
  Cons(1, Nil)          1::[]
  Cons(1, Cons(2, Nil)) 1::2::[]

Задачи хотят :
["Мелещенко"; "Ланской"; "Файнерман"]

Леопардовые жабы
__________________________
\                         \
___________________________

Алгоритм маркова:
1^ -> ^0
0^ -> .1
^ -> .1
*1 ->1*
*0 ->0*
1*-> ^0
0*->.1
e -< *
*)


let rec print_deserved l =
  match l with
    [] -> print_string"К сожалению пока всё."
    | h::t -> print_string h;
                    print_string "Заслужил 2";
                    print_deserved l;;


let rec length l =
  match l with
    [] -> 0
    |h::t -> 1 + length t;;

let rec (@) a b =
  match a with
    [] -> b
    |h::t -> h::(t @ b);;

let rec reverse l =
  match l with
    [] -> []
    |h::t -> (reverse t) @ [h]
    |[] -> failwith "Not a Number";; (*милая хрень*)

let rec inc_ a =
  match a with
    [] -> [1]
    | 0::t -> 1::t
    |1::t -> 0::inc_ t
    |[] -> failwith "Not a Number";;

let inc l =
  reverse(inc_(reverse l));;
(*
let a = [1; 2; 3];;
print_int(length a);;
print_string"\n";;*)
(* 2) *)
let rec print_2 a =
  match a with
    [] -> print_string"\n"
    | h::t -> print_int h ;
              print_2 t;;

let rec minus_1 a =
  match a with
    [0] -> [-1]
    |1::t -> 0::t
    |0::t -> 1::minus_1 t
    |[] -> failwith "Not a Number";;

let minus a = reverse(minus_1(reverse a));;

let plus_2 a = inc(inc(a));;
let is_even a =
  match reverse a with
    0::t -> true
    |1::t -> false
    |[] -> failwith "Not a Number";;

let is_one a =
  match a with
    [1] -> true
    |[0] -> false
    |1::t -> false
    |[] -> failwith "Not a Number";;

let rec cut a =
  match a with
    [0] -> [0]
    |1::t -> 1::t
    |0::t -> cut t
    |[] -> failwith "Not a Number";;

let x = [0; 0; 0; 0; 0; 0; 0; 1; 0; 0; 0; 0; 0];;
let a = cut x;;
print_string "Начальное число(без ведущих нулей) : ";;
print_2 a;;
print_string "'перевёрнутое' число : ";;
print_2 (reverse a);;
print_string "число + 1 : ";;
print_2 (inc a);;
print_string "число - 1 : ";;
print_2 (minus a);;
print_string "число + 2 : ";;
print_2 (plus_2 a);;
if is_even a then
  (
  Printf.printf " чётная\n";
  ) else (
  Printf.printf " нечётная\n";
  );;
if is_one a then
  (
  Printf.printf " = 1\n";
  ) else (
  Printf.printf " != 1\n";
  );;
(*д/з
(1)сделать безквадратный и быстрый реверс.(НЕ ВИЖУ СМЫСЛА, ВЕДЬ ПРОСТОЙ СПРАВЛЯЕТСЯ С КОСМИЧЕСКОЙ СКОРОСТЬЮ)
(2)двоичное число печать(двоичное число -- список)
(3)reverse(ПОДОЙДЁТ И ПРОСТОЙ)
(4)+1
(5)-1
(6)+2
(7)делится ли на 2
(8)проверить равно ли число 1
(9)устранить ведущие нули
*)
