type units =
  Mikro of float
  |Mili of float
  |One of float
  |Kilo of float
  |Mega of float
  |Giga of float
  |Terra of float
  |Peta of float;;

let norm x =
  match x with
    Mikro x -> x /. 1000000.
    |Mili x -> x /. 1000.
    |One x -> x
    |Kilo x -> x *. 1000.
    |Mega x -> x *. 1000000.
    |Giga x -> x *. 1000000000.
    |Terra x -> x *. 1000000000000.
    |Peta x -> x *. 1000000000000000.

let x = read_float();;
print_float(norm (Mikro x));; (*Здесь указывается тип*)
