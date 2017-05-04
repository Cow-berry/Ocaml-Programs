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

let rec plk br =
  match br with
    Leaf i ->  [i]
    |Node(br', br'', i) ->
              plk br''@
              plk br'@
              [i];;

let rec lkp br =
  match br with
    Leaf i ->  [i]
    |Node(br', br'', i) ->
              lkp br'@
              [i]@
              lkp br''
              ;;

let rec pkl br =
  match br with
    Leaf i ->  [i]
    |Node(br', br'', i) ->
              pkl br''@
              [i]@
              pkl br'@
              ;;


let a = Node(Node(Node(Leaf(1), Leaf(2), 7), Leaf(4), 8), Node(Leaf(3), Leaf(5), 9), 6);;
lkp (a);;
