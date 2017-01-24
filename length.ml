type length =
  Apes of float
  |Mummonths of float
  |Pythons of float
  |Birds of float;;

let l1 = read_float();;
let t1 = read_string();;
let l2 = read_float();;
let t1 = read_string();;


let is_longer l1 l2 t1 t2 =
  match(t1, t2 )
