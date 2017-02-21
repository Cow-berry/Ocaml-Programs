let r = [("1","1");("1", "2");("2", "3  ")];;

let is_ref r =
  List.for_all (fun (x, y) ->
    List.mem (x, x) r && List.mem (y, y) r) r;;

Printf.printf "%B\n" (is_ref r);;
