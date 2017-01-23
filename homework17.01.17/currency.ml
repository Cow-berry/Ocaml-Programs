type currency =
  RUB of float|
  USD of float;;
let course = 59.5605;;
let rub = 10.;;
let usd = 15.;;

let print_rub c = Printf.printf "rub : %f \n" c;;
let print_usd c = Printf.printf "usd : %f \n" c;;

let in_rub c =
  if (c <= usd && c >= 0.) then (
    Printf.printf "Вы перевели в рубли %f долларов.\n Сейчас на балансе : \n %f рублей\n %f долларов\n" c (rub +. c *. course) (usd -. c)
    ) else if c < 0. then (
      print_string"Вы ввели отрицательное значение перевода.\n"
    ) else (
      print_string "У вас на балансе нет столько долларов.\n"
    );;

let in_usd c =
  if (c <= rub && c >= 0.) then (
    Printf.printf "Вы перевели в доллары %f рублей.\n Сейчас на балансе : \n %f рублей\n %f долларов\n" c (rub -. c) (usd +. c /. course)
    ) else if c < 0. then (
      print_string"Вы ввели отрицательное значение перевода.\n"
    ) else
    (print_string "У вас на балансе нет столько рублей.\n");;

print_rub rub;;
print_usd usd;;
print_string "Сколько вы хотите перевести из долларов в рубли : "
let c = read_float();;
in_rub c;;
if (c <= usd && c >= 0) then (
  rub = rub +. c *. course;;
  usd = usd -. c;;
  );;
print_string "Сколько вы хотите перевести из рублей в доллары : "
let c = read_float();;
in_usd c;;
if (c <= rub && c >= 0.) then (
  let rub = rub -. c;;
  let usd = usd +. c /. course;;
  );;
