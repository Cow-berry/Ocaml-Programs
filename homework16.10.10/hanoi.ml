let rec move src dst aux n =
    if n = 0 then ()
    else (
        move src aux dst (n-1);
        Printf.printf "%s -> %s\n" src dst;
        move aux dst src (n-1)
    );;

let rec a_move_a src dst aux n =
    if n = 0 then ()
    else (
        a_move_a src aux dst (n-1);
        if src = "A" ||dst = "A" then
            Printf.printf "%s -> %s\n" src dst;
        a_move_a aux dst src (n-1)
    );;

let rec a_move src dst aux n =
    if n = 0 then ()
    else (
        a_move src aux dst (n-1);
        if src = "A" then
          Printf.printf "%s -> %s\n" src dst;
        a_move aux dst src (n-1)
    );;

let rec move_int src dst aux n a b c= (*a = n; b = c = 0*)
    if n = 0 then ()
    else (
        move_int src aux dst (n-1) (a-1) b (c+1);
        Printf.printf "(%i, %i, %i) %s -> %s (%i, %i, %i) \n"(a+1) b c src dst a (b+1) c;
        move_int aux dst src (n-1) a (b+1) (c-1)
    );;

let rec repeat_chars n symbol =
    if n>0 then (
        print_string symbol;
        repeat_chars (n-1) symbol
    );;

let rec pins i n k = (*k = 3*)
    if k > 0 then (
        pins i n (k-1);
        if k = 1 then (
            repeat_chars (n-i) " ";
            repeat_chars i "-";
            print_string "|";
            repeat_chars i "-";
            repeat_chars (n-i+1) " ";
        )
        else (
            repeat_chars n " ";
            print_string "|";
            repeat_chars (n+1) " ";
        )
    );;

let rec pins_draw i  n = (*n = i = src + 1*)(**)
    if i >= 0 then (
        pins_draw (i-1) n;
        pins i n 3;
        print_string "\n";
    );;

let han_draw n =
    pins_draw n n;
    repeat_chars (6*n+5) "-";
    print_string "\n";;


let n = read_int();;
print_string "Все перемещения :\n";;
move "A" "B" "C" n;;
print_string "Перемещения с <А> или на <A> :\n";;
a_move_a "A" "B" "C" n;;
print_string "Перемещения с <A> :\n";;
a_move "A" "B" "C" n;;
print_string "Изначальная позиция :\n";;
han_draw n;;
print_string "Все перемещения с к-вом дисков на штырях: (в стадии разработки)\n";;
move_int "A" "B" "C" n n 0 0;;
