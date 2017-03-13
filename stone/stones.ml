let n1 = read_int();;
let n2 = read_int();;

let stone_draw n1 n2 =
    print_string "1) ";
    for i = 1 to n1 do
      print_string "O " done;
    print_string "\n2) ";
    for i = 1 to n2 do
      print_string "O " done;
      print_string "\n";;

let rec step n k n1 n2 =
  if k = 1 then
    if n <= n1 then
      stone_draw (n1 -n) n2
    else
      (
      Printf.printf "В выбранной кучке нет такого количесства камней. Введите их заново.\n";
      Printf.printf "Введите номер кучки от 1 до 2 : ";
      let k' = read_int() in
      if k' = 1 then
        Printf.printf "Введите количество камней от 1 до %d : " n1
      else
        Printf.printf "Введите количество камней от 1 до %d : " n2;
      let n' = read_int() in
      step n' k' n1 n2;
      )
  else
    if n <= n2 then
      stone_draw n1 (n2 - n)
    else
      (
      Printf.printf "В выбранной кучке нет такого количесства камней. Введите их заново.\n";
      Printf.printf "Введите номер кучки от 0 до 1 : ";
      let k = read_int() in
      if k = 0 then
        Printf.printf "Введите количество камней от 1 до %d :" n1
      else
        Printf.printf "Введите количество камней от 1 до %d :" n2;
      );;

let rec game n1 n2 p_past p_now =
    if ((n1=0) && (n2 = 0)) then Printf.printf "Игорок %d выигрывает!!\n" (p_past)
    else
    (
      Printf.printf "Игрок %d введите номер кучки из которой собираетесь брать камни : " (p_now);
      let k = read_int() in
      if (k = 1) && (n1 = 0) then
      (
        Printf.printf "Эта кучка пуста.";
        let k = 2 in
        Printf.printf "Введите количество камней от 1 до %d. Они будут взяты из второй кучки : " (n2);
        let n = read_int() in
        step n k n1 n2;
        if k = 1  then game (n1 - n) n2 p_now p_past
        else game n1 (n2 - n) p_now p_past
      )
      else if (k = 2) && (n2 = 0) then
      (
        Printf.printf "Эта кучка пуста.";
        let k = 1 in
        Printf.printf "Введите количество камней от 1 до %d. Они будут взяты из второй кучки : " (n1);
        let n = read_int() in
        step n k n1 n2;
        if k = 1  then game (n1 - n) n2 p_now p_past
        else game n1 (n2 - n) p_now p_past
      )
      else
      (
        if k = 1 then
        Printf.printf "Также введите количество камней от 1 до %d : " n1
      else
        Printf.printf "Также введите количество камней от 1 до %d :" n2;
      let n = read_int() in
      step n k n1 n2;
      if k = 1  then game (n1 - n) n2 p_now p_past
      else game n1 (n2 - n) p_now p_past
      );

    );;

(*------------------------------------------------------------------------------------------------------------------------------------------------*)
stone_draw n1 n2;;
game n1 n2 2 1;;
