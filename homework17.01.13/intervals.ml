type crossing = None
  |Point of int
  |Int of (int*int);;

let min a b =
  if a > b then b
  else a;;

let max a b =
  if a > b then a
  else b;;

let crossing x1 x2 y1 y2=
  let b = max x1 y1 in
  let e = min x2 y2 in
  match (b<e, b=e ) with
    (false, false) -> None
    |(false, true) -> Point(b)
    |(true, _) -> Int(b, e);;

let print_crossing c =
  match c with
    None -> print_string "[]"
    |Point x -> Printf.printf "[%d]" x
    |Int (b, e) -> Printf.printf "[%d, %d]" b e;;

print_string "Введите начало 1-го интервала, конец 1-го интервала, начало 2-го итервала, конец 2-го интервала. После каждого ввода переводите стороку. \n";;
let x1 = read_int();;
let x2 = read_int();;

if x1 = x2 then Printf.printf "Введённый вами 'интервал' [%d, %d] является точкой. Пожалуйста перезапустите программу и попробуйте ещё раз(прекратить выполнение программы : Ctrl + C).\n" x1 x2
else if x2 < x1 then Printf.printf "Интервал [%d, %d] имеет длинну меньшую нуля. Разработчик не умеет работать с несуществующими интервалами. Пожалуйста перезапустите программу и попробуйте ещё раз(прекратить выполнение программы : Ctrl + C).\n" x1 x2;;

let y1 = read_int();;
let y2 = read_int();;

if y1 = y2 then Printf.printf "Введённый вами 'интервал' [%d, %d] является точкой. Пожалуйста перезапустите программу и попробуйте ещё раз(прекратить выполнение программы : Ctrl + C).\n" y1 y2
else if y2 < y1 then Printf.printf "Интервал [%d, %d] имеет длинну меньшую нуля. Разработчик не умеет работать с несуществующими интервалами. Пожалуйста перезапустите программу и попробуйте ещё раз(прекратить выполнение программы : Ctrl + C).\n" y1 y2;;

let c = crossing x1 x2 y1 y2;;

if (x1 < x2 && y1 < y2) then print_crossing c
else print_string "Я же говорил что вы неправильно ввели интервал(ы).\n"
