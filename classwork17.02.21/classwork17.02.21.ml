let r = [2, 4, 6, 6, 4, 6, 8, 8, 2];;
let r2 = [(2, 2);(8, 4)];
let chyot r =
  List.for_all (fun (x) ->
    x mod 2 = 0) r;;

let min a b =
  if a > b then b
  else a;;

let max a b =
  if a > b then a
  else b;;

let smax l a b =
  match l with
    [] -> 0
    |h::t -> max h (smax t a b);;

let smin l a b =
  match l with
    [] -> 0
    |h::t -> min h (smin t a b);;

let (.) l

let exists l p =
  List.for_all (fun )

Printf.printf "чётность : %B\nесть a*b и a+b : %B\n" (chyot r) (exists  r (8, 4));;
