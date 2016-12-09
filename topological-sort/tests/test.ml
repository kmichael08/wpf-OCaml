(* testerka do zadania sortowanie topologiczne *)
open Mk371148;;

let zle = ref 0 (* liczba bledow *)

let bad result graf =  (* sprawdza poprawnosc zwroconej listy wierzch. *)
  let pos = snd (List.fold_left (fun (nr, m) el -> (nr + 1, PMap.add el nr m)) 
                (0, PMap.empty) result) 
  in
    let logic = ref false in
      List.iter (fun (w, lista) -> 
                   (List.iter (fun el -> if (PMap.find el pos) < (PMap.find w pos) 
                                        then logic := true ) lista)   
                )
      graf;
      !logic    
     
let test nr l dag = 
  try if bad (topol l) l then 
        begin 
          incr zle; 
          Printf.printf "Blad - test nr %d \n" nr 
        end
  with
  | Cykliczne -> if dag then
                   begin
                     incr zle;
                     Printf.printf "Blad - test nr %d \n" nr
                   end;;

let ll = [(1, [2; 3; 4]); (2, [5]); (3, [5]); (4, [3; 5]); (5,[])];;      

let cykl = [(1, [2]); (2, [3]); (3, [4; 6]); (4, []); (5, [3]); (6, [1])];;

let przyklad = [(1, [2; 6]); (2, [3; 4]); (3, [5]); (4, [5]); (5, [6]); (8, [7; 3]); (7, [6])];;


let przyklad2 = [(1, [2]); (2, [1]); (3, [1])];;

test 1 cykl false;;
test 2 przyklad2 false;;
test 3 ll true;;
test 4 przyklad true;;

let l1 = [(17, [420]); (420, [5]); (1801, [300]); (5, [1801]); (300, [17])];;

test 6 l1 false;;

let l2 = [(1000,[])];;
test 7 l2 true;;

let l3 = [("kura",["jajko"]); ("jajko", ["kura"])];;
test 8 l3 false;;

let l4 = [("skarpety", ["buty"]); ("podkoszulka", ["koszula"; "bluza"; "kurtka"]); ("koszula", ["bluza"; "kurtka"]); ("bluza", ["kurtka"])];;

test 9 l4 true;;

let l5 = [('a', ['z'; 'b']); ('b', ['c'; 'd']); 
          ('z', ['b']); ('c', ['d']); ('d', ['e'; 'f'; 'g'])];;

test 10 l5 true;;

let l6 = [(44, [11; 55]); (11, [33; 22]); (33, [22; 55]); (66, [22])];;

test 11 l6 false;;

let l7 = [(2000, [150; 17]); (150, [1]); (42, [8]); (8, [17; 2000]); (17, [3]); (3, [42])];;

test 12 l7 false;;

let l8 = [(1, [2]); (3, [4]); (4, [5]); (5000, [6000]); (6000, [7000]); (6, [7; 8]);
          (20, [21; 22]); (21, [24]); (22, [23]); 
          (24, [22; 25; 26]); (26, [25]); (25, [23])];;

test 13 l8 true;;

let l9 = [(1, [2]); (2, [100; 3]); (3, [1000]); (1000, [100])];;

test 14 l9 false;;

let l10 = [('a', ['b'; 'd']); ('b', ['c'; 'd']); ('c', ['d'])];;

test 15 l10 true;;

let l11 = [(12, [50; 300]); (300, [50])];;

test 16 l11 true;;

let l12 = [(1, [1])];;

test 17 l12 false;;

let l13 = [(3, [80])];;

test 18 l13 true;;

let l14 = [(1000, [6; 4; 5; 3; 2])];;

test 19 l14 true;;

let l15 = [(2, [1]); (3, [1]); (4, [1]); (6, [1]); (5, [1])];;

test 20 l15 true;;

let l16 = [(2, [1; 3]); (3, [1; 4]); (4, [1; 5]); (5, [1; 6]); (6, [1; 2])];;

test 21 l16 false;;

let l17 = [(2, [3]); (3, [1]); (1, [2])];;

test 22 l16 false;;


let _ =
  if !zle = 0 then Printf.printf "Wszystko ok!! \n"
  else Printf.printf "Liczba bledow to : %d \n" !zle
 
