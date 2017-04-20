let add_unique r1 r2 = r2 @ (List.filter(fun x -> not (List.mem x r2))r1);;
let sim' r = List.map(fun(x, y) -> (y, x)) r;;
let sim r = add_unique (sim' r) r;;
let r = [(0, 0);(1, 2);(2,3);(1,5)];;
let r_sim = sim r;;
let print_list r = List.iter(fun (x, y) -> Printf.printf "(%d, %d)" x y) r;;
print_string  "*\n";;
print_list r_sim;;
print_string"*\n";;
