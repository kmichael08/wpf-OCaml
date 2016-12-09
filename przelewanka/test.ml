(* Testy - przelewanka Michal Kuzba *)

open Przelewanka

let zle = ref 0

let test nr tab result =
  if przelewanka tab <> result then
    begin
      Printf.printf "Blad - test nr %d \n" nr;
      incr zle
    end
  else Printf.printf "OK - test nr %d \n" nr;;

(* brzegowe przypadki *)
test 1 [||] 0;;
test 2 [|(0, 0)|] 0;;
test 3 [|(5, 0)|] 0;;
test 4 [|(5, 5)|] 1;;
test 5 [|(7, 3)|] (-1);;
test 6 [|(8, 4)|] (-1);;
test 7 [|(0, 0); (0, 0)|] 0;;

(* nasze proste warunki na nie *)
let t1 = [|(8, 3); (5, 2); (20, 7)|]
let t2 = [|(14, 0); (21, 2)|]
let t3 = [|(42, 42); (30, 10); (12, 6)|];;

test 8 t1 (-1);;
test 9 t2 (-1);;
test 10 t2 (-1);;

(* male testy *)
let t4 = [|(42, 30); (30, 24); (72, 0)|]
let t5 = [|(8, 8); (14, 10)|]
let t6 = [|(8, 0); (14, 8)|];;

test 11 t5 9;;
test 12 t6 2;;
test 13 t4 9;;

let t7 = [|(24, 24); (40, 8)|];;
test 14 t7 7;;

(* wieksze testy *)

let krl = [|(10,3);(4,2);(7,2);(2,1);(8,6);(5,3);(1,0)|];;
test 15 krl 9;;

let tab1 = [|(20, 20); (10, 10); (30, 10)|];;
test 16 tab1 3;;

let tab2 = [|(80, 40); (40, 10); (30, 10); (20, 10); (10, 10)|];;
test 17 tab2 5;;

let tab3 = [|(190, 70); (33, 19); (240, 50); (120, 50); (70, 37); (14, 14)|];;
test 18 tab3 6;;

let tab4 = [|(190, 70); (33, 19); (6, 6); (240, 50); (120, 50); (70, 37); (14, 8)|];;
test 19 tab4 7;;

let _ =
  if !zle = 0 then Printf.printf "Testy - ok \n"
  else Printf.printf "Liczba bledow - %d \n" !zle
