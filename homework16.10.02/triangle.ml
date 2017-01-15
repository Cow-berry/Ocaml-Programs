let rec fact a =
  if a <= 1 then 1
  else a * fact (a-1);;

let rec pasElem n k =
  (fact n) / (fact k * fact (n-k));;

let rec paskal_line n k =
  if k<=n then (
    Printf.printf "%6d" (pasElem n k);
    paskal_line n (k+1);
  )
  else print_string "\n";;

let rec print_spaces k =
  Printf.printf "%*S" k "";;

let rec triangle n ns =
  if n <= ns then (
    paskal_line n 1;
    triangle (n+1) ns;
  );;


let a = read_int();;
let b = read_int();;
triangle a b;;
print_spaces 1;;
triangle a b;;
