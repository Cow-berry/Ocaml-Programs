let rec count n cnt base digit =
    if n <= 0 then cnt
    else
      if (n mod base) = digit then
        count (n/base) (cnt + 1) base digit
      else
        count (n/base) cnt base digit;;

let ten n =
  count (abs n) 0 10 1 ;;

let three n =
    count (abs n) 0 3 1;;

let n = read_int();;
Printf.printf " в числе %d  %d  единиц\n" n (ten n) ;;
Printf.printf "Введите пожалуйста  число в 3-ой системе исчисления (цифры : 0, 1, 2)\n"
let m = read_int();;
Printf.printf "в числе %d в троичной системе счисления %d едениц\n" m (three m);;
