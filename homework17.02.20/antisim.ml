let r = [("1", "2");("2", "1")];;

let antisim r =
  List.for_all (fun (x, y) ->
    (List.mem (y, x) r && x = y) || not (List.mem (y, x) r)
  ) r;;

Printf.printf "%B\n" (antisim r);;
