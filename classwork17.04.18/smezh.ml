let transform' l i = (*список пар int и  номер строки*)
  List.filter (fun (x, y) -> y = i) l;;

let rec transform l n = (*список пар int и сторону квадрата*)
  if n <= 0 then []
  else
    match l with
      [] -> []
      |_ ->
        match transform l (n-1) with
          [] -> (transform' l n) @ (transform l (n-1)
          |_ -> (transform' l n) :: (transform l (n-1));;

let rec transformTF' l = (*список пар int*)
  match l with
    []->[]
    |h::t ->
      if List.mem h l then true :: transformTF' t
      else false :: transformTF' t;;


let rec transformTF l = (*список списков пар int*)
  match l with
    []-> []
    |h::t -> (transformTF' h) :: (transformTF t);;

let tr r n=
  transformTF (transform r n );;

let rec print' l =
  match l with
    [] -> ()
    |h::t -> if h then print_string "* "
    else print_string "  ";;

let rec print l =
  match l with
    []->()
    |h::t ->
      print' h;
      print_string"\n";
      print t;;

let r = [(0, 0);(0, 1);(0, 2);(1, 2);(2, 1)];;

let l = tr r 3;;
print_string"123890\n";;
print l;;
