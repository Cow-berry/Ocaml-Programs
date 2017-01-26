type length =
  Apes of float
  |Mummonths of float
  |Pythons of float
  |Birds of float;;

let mummonths = 1. /. 4.;;
let apes = 1. /. 26.;;
let birds = 1. /. 38.;;

let len x =
  match x with
    Apes x -> (apes *. x)
    |Mummonths x -> mummonths *. x
    |Birds x -> birds *.x
    |Pythons x -> x;;

let print_length x =
  match x with
    Apes x -> Printf.printf "%f Обезьян" x
    |Mummonths x -> Printf.printf "%f Мамонтов" x
    |Pythons x -> Printf.printf "%f Питонов" x
    |Birds x -> Printf.printf "%f Попугаев" x;;

let max a b =
  if len a >= len b then a
  else b;;

let x = read_float();;
let y = read_float();;
let x_type = Birds x;; (*Здесь указываются типы*)
let y_type = Pythons y;;
if len x_type = len y_type then
  (
    print_length x_type;
    print_string " = ";
    print_length y_type;
  ) else if max x_type y_type = x_type then
  (
    print_length x_type;
    print_string " > ";
    print_length y_type;
  ) else if max x_type y_type = y_type then
  (
    print_length y_type;
    print_string " > ";
    print_length x_type;
  );;
