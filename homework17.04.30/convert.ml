type br_tree =
  Br of br_tree list;;
(*
Nil -> Br[]
Node(Nil, Nil) -> Br[Br[];Br[]]
Node(Node(Nil, Nil), Nil)-> Br[Br[Br[];Br[]];Br[]]
*)
type binary_tree =
  Nil
  |Node of binary_tree * binary_tree;;

let rec binaryInBr (*(Node(a, b))*) bin =
  match bin with
    Nil -> Br[]
    |Node(Nil, Nil) -> Br[Br[]; Br[]]
    |Node(Node(a', b'), Nil) -> Br[binaryInBr(Node(a', b'));Br[]]
    |Node(Nil, Node(a', b')) -> Br[Br[];binaryInBr(Node(a', b'))]
    |Node(Node(a', b'), Node(c', d')) -> Br[binaryInBr(Node(a', b'));binaryInBr(Node(c', d'))] ;;

let rec isBinary (Br t) =
  if t = [] then true
  else if t = [Br[]] then false
  else if List.length t > 2 then false
  else (isBinary (List.nth t 0)) && (isBinary (List.nth t 1));;

let rec brInBinary' (Br t) =
  match t with
    [] -> Nil
    |[Br t'; Br t''] -> Node(brInBinary' (Br t'), brInBinary' (Br t''));;

let brInBinary br =
  let is = isBinary br in
  match is with
    true -> brInBinary' br
    |false -> failwith("Not Binary");;
