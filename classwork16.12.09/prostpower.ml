

(*let rec power n m =
    if (m*m)=n then true;
    if m>n then false
    else power n (m+1);;
*)
let rec power m p =
    if p>0 then power m (p-1)
    else 1;;

let rec djenialno n m p =
    if (power m p = n)||(power m p < n && djenialno  n (m+1) p);;

let n = read_int();;
djenialno   
