type currency =
  Rub of float|
  Usd of float;;
let course = 59.5605;;
let rub = 10.;;
let usd = 15.;;

let print_currency c =
  match c with
    Rub x -> print_float(x)
    |Usd x -> print_float(x);;

let in_usd c =
  match c with
    Rub (x) -> x /. course
    |Usd (x) -> x ;;

let in_rub c =
  match c with
    Rub (x) -> x
    |Usd (x) -> x *. course;;

let add_part c1 c2 =
  match (c1, c2) with
    (Rub x, Rub y) -> Rub(x+.y)
    |(Usd x, Usd y) -> Usd(x+.y)
    |(Rub x, Usd y) -> Rub (x+.y*.course)
    |(Usd x, Rub y) -> Rub (x*.course+.y);;

let add c1 c2 t1 t2 =
  match (t1, t2) with
    ("r", "r") -> add_part(Rub c1, Rub c2)
    |("u", "u") -> add_part(Usd c1, Usd c2)
    |("r", "u") -> add_part(Rub c1, Usd c2)
    |("u", "r") -> add_part(Usd c1, Rub c2);;


print_string "Введите 1 если хотите перевести рубли в доллары. 2 если хотите перевести доллары в рубли."
let q = read_int();;
if q = 1 then
  let x = read_float() in
  print_float(in_usd (Rub x))
else if q = 2 then
  let x = read_float() in
  print_float(in_rub (Usd x));;

print_string"Типы : r и u";;
let t1 = read_string();;
let t2 = read_string();;
let c1 = read_float();;
let c2 = read_float();;
print_currency(add c1 c2 t1 t2);;
