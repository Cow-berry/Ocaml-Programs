type bin_tree =
  Leaf of int
  |Node of bin_tree * bin_tree * int;;

let rec lpk br =
  match br with
    Leaf i ->  [i]
    |Node(br', br'', i) ->
              lpk br'@
              lpk br''@
              [i];;

let rec lkp br =
  match br with
    Leaf i ->  [i]
    |Node(br', br'', i) ->
              lkp br'@
              [i]@
              lkp br''
              ;;

let rec cut v r l =
match l with
    |h::t -> if v <> h then cut v (r@[h]) t
            else (r, t)
    | _ -> failwith "v not found";;

let rec cut_nth n r l =
    if n = 0 then (r, l)
    else cut_nth (n-1) (r@[List.hd l]) (List.tl l);;


let rec restore lkp lpk =
  match (lkp, lpk) with
    (h1::[], h2::[]) -> Leaf(h1)
    |_ -> let k = List.nth lpk ((List.length lpk) - 1) in
          let (l_lkp, r_lkp) = cut k [] lkp in
          let (l_lpk, r_lpk_k) = cut_nth (List.length l_lkp) [] lpk in
          let r_lpk = List.rev (List.tl (List.rev r_lpk_k)) in
          Node(restore l_lkp l_lpk, restore r_lkp r_lpk, k);;


let tree = Node(Node(Leaf(3), Node(Leaf(7), Node(Leaf(9), Leaf(10), 8), 4), 1), Node(Leaf(5), Leaf(6), 2), 0);;
