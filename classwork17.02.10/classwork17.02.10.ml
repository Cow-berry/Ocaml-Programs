
(*
Другие языки

type gender = (Male, Female, Neuter)

for i = male to neuter do
begin
.
.
.
end;

Грамматики

A|
 V
*)
set
Graphics.open_graph " 700x700";;
Graphics.moveto 100 100;;
Graphics.lineto 300 300;;
Graphics.draw_ellipse 100 100 50 100;;
Graphics.draw_circle 300 300 50;;

type cmd = L|D|R;;

for i = 1 to 50000 do
  Graphics.draw_circle (Random.int 500) (Random.int 500) (Random.int 50) done;;

let _ = Graphics.read_key ();;
(*
дз
1) 500 случайных кружочков
2) Ломанная из 500 линий
3) Нарисовать спираль
4) n-угольник(правильный)
*)
