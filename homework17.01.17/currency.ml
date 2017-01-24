type currency =
  Rub of float|
  Usd of float;;
let course = 59.5605;;
let rub = 10.;;
let usd = 15.;;



let in_usd c =
  match c with
    Rub (x) -> x /. course
    |Usd (x) -> x ;;

let in_rub c =
  match c with
    Rub (x) -> x
    |Usd (x) -> x *. course;;

print_string "Введите 1 если хотите перевести рубли в доллары. 2 если хотите перевести доллары в рубли."
let q = read_int();;
if q = 1 then
  let x = read_float() in
  print_float(in_usd (Rub x))
else if q = 2 then
  let x = read_float() in
  print_float(in_rub (Usd x));;
