type direction = None
  |AB
  |BA
  |All;;
(*Семафор :
0-красный
1-жёлтый
2-жёлтый+зелёный
3-зелёный*)
let direction a b =
  match (a=b+1, b=a+1, a=3, b=3) with
    (true, false, _, false) -> AB
    |(false, true, false, _) -> BA
    |(false, false, true, true) -> All
    |(false, false, false, false) -> None
    |(false, false, false, true) -> None
    |(false, false, true, false) -> None;;

let print_direction d =
  match d with
    None -> print_string"Такого не может быть(видимо кто-то накосячил в программе сeмафора).\n"
    |AB -> print_string"A~>B\n"
    |BA -> print_string"A<~B\n"
    |All -> print_string"Определение направления невозможно. Подождите, пока состояние одного из них не изменится.\n";;

print_string "По очереди введите состояние первого(А) и второго(В) семафора.\n  0 -- красный цвет \n  1 -- жёлтый цвет\n  2 -- жёлтый + зелёный цвет \n  3--зелёный\n";;
let a = read_int();;
let b = read_int();;
if (a<0 || b<0 || a>3 || b>3) then
  print_string "Вы введи несуществующее состояние семафора."
else
  let d = direction a b in print_direction d
  ;;
