let x = read_line();;
let y = read_line();;
let op = read_line();;

match x with
"true" -> true
| "True"-> true
| "1" -> true
| "false" -> false
| "False" -> false
| "0" -> false
| _ -> failwith "not bool"
