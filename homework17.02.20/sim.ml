let r = [("1", "2");("2", "1");("1", "3")];;

let is_sim r =
    List.for_all (fun (x, y) ->
      x=y || List.mem (y, x) r
    ) r;;

Printf.printf "%B\n" (is_sim r);;
