let rec step n m =
    if n <= (m*m) then
          step n (m-1)
    else (m+1);;

let rec repeat_chars n symbol =
  if n>0 then (
    print_string symbol;
    repeat_chars (n-1) symbol
  );;

let space n =
  repeat_chars n " ";;

let rec one n =
  repeat_chars n "1";;

let line spaces ones =
  space spaces;
  one ones;
  print_string "\n";;

let rec triangle lines n r =
    if lines > 1 then (
      line (lines-1) n;
      triangle (lines-1) (n+2) r;
    )
    else line 0 r ;;

let n = read_int();;
let l = step n n;;
let r = n - (l-1) * (l-1);;
triangle l 1 r;;
