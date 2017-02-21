let r =[(1, 2);(2, 3); (1, 3)];;

let is_line r =
  List.for_all (fun (x, y) ->
    x<=y || y<=x) r;;

Printf.printf "%B\n" (is_line r);;
