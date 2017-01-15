(*АаАААААаааАааАаАаАаАаААаааАаАаАаАААаАаАаАааАааааааАаАаАаАааааааааааАаАаАаАаАаАаАаАаАаААаАаА как это делать???????????????????????????*)
let rec move src dst aux n =
    if n = 0 then ()
    else (
        move src aux dst (n-1);
        Printf.printf "%s -> %s\n" src dst;
        move aux dst src (n-1)
    );;

let rec move src dst aux n =
    if n = 0 then ()
    else (
        if src = "A" || dst = "A" then return True
        else return False
        move src aux dst (n-1);
        Printf.printf "%s -> %s\n" src dst;
        if aux = "A" || dst = "A" then return True
        else return False
        move aux dst src (n-1)
    );;

let n = read_int();;
max "A" "B" "C" n 0;;
move "A" "B" "C" n ;;
