let r = [("1", "2");("2", "1");("3", "4")];;

let is_assim r =
  List.for_all (fun (x, y) ->
    not (List.mem (y, x) r)) r;;

Printf.printf "%B\n" (is_assim r);;
