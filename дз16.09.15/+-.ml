let minus a = a-1;;
let plus a = a+1;;

print_string "Введите число:" ;;
let x = read_int();;
print_int (plus x);;
print_string ", ";;
print_int (minus x);;
print_string "\n";;