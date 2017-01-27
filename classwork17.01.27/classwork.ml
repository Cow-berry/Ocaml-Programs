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
    |h::t -> (reverse t) @ [h];; (*милая хрень*)

let rec inc_ a =
  match a with
    [] -> [1]
    | 0::t -> 1::t
    |1::t -> 0::inc_ t;;

let inc l =
  reverse(inc_(reverse l));;

let a = [1; 2; 3];;
print_int(length a);;
print_string"\n";;
