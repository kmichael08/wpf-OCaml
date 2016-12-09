(* ==================================================================================== *)
(*                            Zadanie Drzewa Lewicowe                                   *)
(*                              Autor : Michał Kuźba                                    *)
(*                             Reviewer : Amadeusz Iwanowski                            *)
(* ==================================================================================== *)
(* typ - kolejka *)
(* lewe poddrzewo, wartosc w korzeniu, prawe poddrzewo, dlugosc skrajnie prawej sciezki *)
type 'a queue  = Pusta | Node of 'a queue * 'a * 'a queue * int;; 

(* wyjatek podnoszony przez [delete_min] gdy kolejka jest pusta  *)
exception Empty;;

(* dlugosc skrajnie prawej sciezki *)
let dlugosc q =
  match q with
  | Pusta              -> 0
  | Node (_, _, _, dl) -> dl

(* laczy poddrzewa tak zeby prawe mialo krotsza skrajnie prawa sciezke *)
let make q1 wartosc q2 = 
  if dlugosc q1 > dlugosc q2 then Node (q1, wartosc, q2, dlugosc q2 + 1)
  else Node (q2, wartosc, q1, dlugosc q1 + 1)

(* Pusta kolejka priorytetowa   *)
let empty = Pusta;;

(* sprawdza czy kolejka jest pusta   *)
let is_empty q =
  match q with
  | Pusta -> true
  | _     -> false;;

(* zlaczenie dwoch kolejek *)
let rec join q1 q2 =
  match q1, q2 with
  | Pusta, _ -> q2
  | _, Pusta -> q1
  | Node (l1, v1, p1, _), Node (l2, v2, p2, _) ->
       if v1 < v2 then 
       let poddrzewo = join p1 q2
       in make l1 v1 poddrzewo
       else let subtree = join q1 p2 
       in make l2 v2 subtree
      
(* zwraca kolejke powstala z dolaczenia elementu e do kolejki q *)
let add e q  = 
  let sub = Node (Pusta, e, Pusta, 0) (* sub - drzewo z 1 el. - e *)
  in join sub q;;


(* zwraca pare (minimum, drzewo z usunietym minimum) *)
let delete_min q =
  match q with
  | Pusta             -> raise Empty 
  | Node (l, v, p, _) -> (v, join l p)
 

