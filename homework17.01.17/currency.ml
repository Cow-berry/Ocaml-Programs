type currency =
  RUB of float|
  USD of float;;
let course = 63.7167;;
let read_rub c =
  match c with
    _ -> RUB(c);;

let read_usd c =
  match c with
    _ -> USD(c);;

let print_currency c =
  match c with
    RUB x -> Printf.printf "Ваш вклад в рублях составляет %f рублей.\n" x
    |USD x -> Printf.printf "Ваш вклад в долларах состовляет %f доллпров.\n" x;;

(*let in_rub c =
  Printf.printf "%f долларов = %f рублей" .с (.с*.course);;

let in_usd c =
  Printf.printf "%f рублей = %f долларов" .с (.с/.course);;*)

let rub = read_float;;
let usd = read_float;;
let r = read_rub rub;;
let u = read_usd usd;;
print_currency r;;
print_currency u;;
