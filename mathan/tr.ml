let ro = [('a', 'a'); ('b', 'b'); ('c', 'c'); ('1', '1'); ('*', '*'); ('a', 'b')];;


let rec is_tr l res =
  if res =  false then false
  else
  (for i = 0 to List.length l - 1 do
    for j = 0 to List.length l -1 do
      for k = 0 to List.length l -1 do
        if snd(List.nth l i) = fst (List.nth l j) then
          if (fst(List.nth l i) != fst(List.nth l k)) || (snd(List.nth l j) != snd(List.nth l k)) then is_tr l false
          else is_tr l true
        else is_tr l true
      done
    done
  done; res);;

let rec is_tr l =
  
Printf.printf"%B\n" (is_tr ro true);;
