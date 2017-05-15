let rec fast_fib x1 x2 n =
  if (n = 1) then x2
  else fast_fib x2 (x1+x2) (n-1);;

let fib n = fast_fib 1 1 n;;
