let ro = [("a", "a");("b", "b");("c", "c");("1", "1");("*", "*");("a", "b");("b", "a");("a", "1");("a", "*");("b", "*")];;

let is_tr r =
  List.for_all (fun (x, y) ->
    List.for_all (fun (y', z) ->
      y <> y' || List.mem (x, z) r
      ) r
    ) r;;

Printf.printf "%B" ( is_tr ro );;
