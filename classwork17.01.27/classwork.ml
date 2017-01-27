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

let a = [1; 2; 3];;
print_int(length a);;
print_string"\n";;
