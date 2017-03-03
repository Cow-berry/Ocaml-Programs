let z = [(5, 7);(2, 4);(3, 5)];;
(*
макс -- больше всех которые сравнимы с ним
наибольший -- который больше всех (сравним со всеми)
*)

let is_srav (x, y) (m, n) =
  if (x>m && y>n) || (x<m && y<n) then true
  else false;;

let is_max r =
  List.for_all (fun (x, y) ->
    List.for_all ( fun (m, n)
      if
    ) r
  ) r;;
  {
    
