let max a b =
  if a < b then b
  else a;;

let min a b =
  if a < b then a
  else b;;

let rec max_list l =
  match l with
    []-> 0
    |h::t -> max h (max_list t);;

let rec min_list l =
  match l with
    [] -> 0
    |h::t -> min h (min_list t);;

let rec sum_list l =
  match l with
    [] -> 0
    |h::t -> h + sum_list t;;

    let rec print_list l =
      match l with
        [] -> print_string"[]"
        |h::t -> print_int h;
                print_list t;;

let average l = (sum_list l) / (List.length l);;(* ????????????????????????????????????????????? *)

let del mi ma l =
  List.map (fun (x) ->
        if (x <> mi) && (x <> ma) then x) l;;

let rec sum_ch l =
  match l with
    [] -> 0
    |h::t ->
        if h mod 2 = 0 then h + sum_ch t
        else sum_ch t;;

let rec sum_plus l =
  match l with
    [] -> 0
    |h::t ->
        if h >=0 then h + sum_plus t
        else sum_plus t;;

let rec multi_minus l =
  match l with
    [] -> 1
    |h::t ->
        if h < 0 then h * multi_minus t
        else multi_minus t;;

let composition l = (sum_plus l) - (multi_minus l)


let l = [1;1;5;3;7;3;-3;-7];;
let mi = min_list l;;
let ma = max_list l;;
Printf.printf "sum_list (l) = %d\n" (sum_list l);;
Printf.printf "average (l) = %d\n" (average l);;
Printf.printf "sum_ch (l) = %d\n" (sum_ch l);;
Printf.printf "composition (l) = %d\n" (composition l);;
let d = del mi ma l;;
print_list d;;
