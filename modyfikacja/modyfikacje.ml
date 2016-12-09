(* ========================================================================= *)
(*                          Zadanie Modyfikacje                              *)
(*  Autor : Micha‚³ Ku‚¼ba  371148                                             *)
(*  Reviewer : Aliaksei Suvorau  374118                                      *)
(* ========================================================================= *)

type  t = Empty | Node of  t * int * int * t * int * int
(* lewe drzewo, przedzial, prawe, wysokosc, ile liczb w poddrzewie  *)

(* =======================  Pomocnicze ==================================== *)

exception Puste

let wys tree = 
  match tree with
  | Node (_, _, _, _, h, _) -> h
  | Empty                   -> 0

let pod tree = (* ile liczb w poddrzewie *)
  match tree with
  | Node (_, _, _, _, _, sub) -> sub
  | Empty                     -> 0

let dl tree = (* dlugosc przedzialu w korzeniu, uzwgl. max_inty *)
  match tree with
  | Node (_, l, p, _, _, _) -> 
      let wyn = p - l + 1 in if wyn <= 0 then max_int else wyn
  | Empty                   -> 0

let rec interval el tree = (* przedzial zawierajacy el *)
  match tree with
  | Node (ltree, l, p, rtree, _, _) ->
      if el >= l && el <= p then (l, p)
      else if el < l then interval el ltree
      else interval el rtree
  | Empty -> (1, -1) (* nie zawiera *)

let mem el tree = (* czy el jest w drzewie *)
  interval el tree <> (1, -1)

let smallest el tree = (* najm. element z przedzialu zawierajacego el *)
  assert (interval el tree <> (1, -1)); (* war. pocz. el zawarte w drzewie *)
  fst (interval el tree)

let largest el tree =  (* najw. element z przedzialu zawierajacego el *)
  assert (interval el tree <> (1, -1)); (* war. pocz. el zawarte w drzewie *)
  snd (interval el tree)

let rec is_avl tree = (* czy drzewo jest avl - do testowania *)
  match tree with 
  | Node (ltree, _, _, rtree, _, _) ->
      is_avl ltree && is_avl rtree && abs (wys ltree - wys rtree) <= 2
  | Empty -> true

let dodaj a b = (* dodaje dwie liczby nieujemne, uwazajac na max_inty *) 
  let sum = a + b in
    if sum < 0 then max_int else sum

let increase liczba = if liczba = max_int then max_int else liczba + 1

let decrease liczba = if liczba = min_int then min_int else liczba - 1

let dod4 a b c d = 
  dodaj (dodaj a b) (dodaj c d)

let make ltree l p rtree = (* tworzy drzewo *)
  let sub = dod4 (pod ltree) (dl ltree) (pod rtree) (dl rtree) in 
    Node (ltree, l, p, rtree, max (wys ltree) (wys rtree) + 1, sub)

let bal l lewy prawy r = (* balansowanie drzewa *)
  let hl = wys l in
  let hr = wys r in
  if hl > hr + 2 then
    match l with
    | Node (ll, llew, lpraw, lr, _, _) ->
        if wys ll >= wys lr then make ll llew lpraw (make lr lewy prawy r)
        else
          (match lr with
          | Node (lrl, lrlew, lrpraw, lrr, _, _) ->
              make (make ll llew lpraw lrl) lrlew lrpraw (make lrr lewy prawy r)
          | Empty -> assert false)
    | Empty -> assert false
  else if hr > hl + 2 then
    match r with
    | Node (rl, rlew, rpraw, rr, _, _) ->
        if wys rr >= wys rl then make (make l lewy prawy rl) rlew rpraw rr
        else
          (match rl with
          | Node (rll, rllew, rlpraw, rlr, _, _) ->
              make (make l lewy prawy rll) rllew rlpraw (make rlr rlew rpraw rr)
          | Empty -> assert false)
    | Empty -> assert false
  else make l lewy prawy r

let rec min_elt = function 
  | Node (Empty, l, p, _, _, _) -> (l, p)
  | Node (ltree, _, _, _, _, _) -> min_elt ltree
  | Empty -> raise Puste

let rec max_elt = function
  | Node (_, l, p, Empty, _, _) -> (l, p)
  | Node (_, _, _, rtree, _, _) -> max_elt rtree
  | Empty -> raise Puste

let rec remove_min_elt = function
  | Node (Empty, _, _, rtree, _, _) -> rtree
  | Node (ltree, l, p, rtree, _, _) -> bal (remove_min_elt ltree) l p rtree
  | Empty -> raise Puste

let empty = Empty

let is_empty tree = tree = Empty

let rec add_max_elt ((lewy, prawy) as interval) tree = 
  (* war. pocz. przedzial jest wiekszy od wszystkich i nie jest sasiedni *)
  assert (is_empty tree || decrease lewy > snd (max_elt tree)); 
  match tree with
  | Empty -> make Empty lewy prawy Empty
  | Node (ltree, l, p, rtree, _, _) ->
      if is_empty rtree then bal ltree l p (make Empty lewy prawy Empty)        
      else bal ltree l p (add_max_elt interval rtree)

let rec add_min_elt ((lewy, prawy) as interval) tree = 
  (* war. pocz. przedzial jest mniejszy od wszystkich i nie jest sasiedni *)
  assert (is_empty tree || increase prawy < fst (min_elt tree)); 
  match tree with
  | Empty -> make Empty lewy prawy Empty
  | Node (ltree, l, p, rtree, _, _) ->
      if is_empty ltree then bal (make Empty lewy prawy Empty) l p rtree
      else bal (add_min_elt interval ltree) l p rtree


let merge t1 t2 = (* laczy dwa poddrzewa *)
  match t1, t2 with
  | Empty, _ -> t2
  | _, Empty -> t1
  | _ ->  let sm = min_elt t2 in
            bal t1 (fst sm) (snd sm) (remove_min_elt t2)
  
(* ========================== Glowne ======================================== *)

let rec remove ((lewy, prawy) as interval) tree = 
  (* assert (is_avl tree); *) (* war. pocz. - drzewo jest avl *)
  if lewy > prawy then tree 
  else match tree with
    | Node (ltree, l, p, rtree, _, _) ->
        if lewy > p then
          bal ltree l p (remove interval rtree)
        else if prawy < l then
          bal (remove interval ltree) l p rtree
        else if lewy > l && prawy < p then
          begin 
            if wys ltree < wys rtree then
              let tmp = add_max_elt (l, decrease lewy) ltree in
                let one = add_max_elt (increase prawy, p) tmp in 
                  merge one rtree
            else 
              let tmp = add_min_elt (increase prawy, p ) rtree in
                let two = add_min_elt (l, decrease lewy) tmp in
                   merge ltree two       
          end
        (* usuwamy z srodka przedzialu *)
        (* przedzialy powstale po wycieciu, dodajemy do krotszego z poddrzew *)
        else if lewy > l then 
          bal ltree l (decrease lewy) (remove interval rtree)
        else if prawy < p then
          bal (remove interval ltree) (increase prawy) p rtree
        (* przypadki gdy przedzialy czesciowo sie nachodza *)
        else merge (remove interval ltree) (remove interval rtree) 
        (* usuwamy caly przedzial *)
    | Empty -> Empty


let rec add ((lewy, prawy) as interval) t = 
  if lewy > prawy then t
  else
  match t with
  | Node (ltree, l, p, rtree, _, _) ->  
      if lewy > (increase p) then let nowy = add interval rtree in
        bal ltree l p nowy
      else if prawy < decrease l then let nowy = add interval ltree in
        bal nowy l p rtree
          (* rozlaczne calkowicie z przedzialem *)
      else if lewy >= l && prawy <= p then t 
          (* przedzial zawiera sie w istniejacym *)
      else if lewy = (increase p) then 
        begin
          let limit = 
            if mem (increase prawy) t then largest (increase prawy) t else prawy in 
              let nowy = remove (lewy, limit) rtree in
                bal ltree l limit nowy
        end   
      else if prawy = decrease l then
        begin
          let limit =
            if mem (decrease lewy) t then smallest (decrease lewy) t else lewy in
              let nowy = remove (limit, prawy) ltree in
                bal nowy limit p rtree
        end 
      else if lewy >= l then add ((increase p), prawy) t
      else if prawy <= p then add (lewy, (decrease l)) t
      else let pierwszy = add (lewy, (decrease l)) t 
             in add ((increase p), prawy) pierwszy
  | Empty -> make Empty lewy prawy Empty

(* w add jesli przedzialy sie pokrywaja to dodajemy przedzial /
 jego przedluzenie jesli koniec zawiera sie w poddrzewie, a nastepnie 
 usuwamy ten przedzial z poddrzewa - uzywajac remove *)    

let rec join ltree ((l, p) as interval) rtree = 
   (* laczymy dwa drzewa roznych wysokosci *)
  match ltree, rtree with
  | Empty, _ -> add interval rtree
  | _, Empty -> add interval ltree
  | Node (lt1, l1, p1, pt1, h1, _), Node (lt2, l2, p2, pt2, h2, _) ->
      if h1 > h2 + 2 then 
        bal lt1 l1 p1 (join pt1 interval rtree)
      else if h2 > h1 + 2 then
        bal (join ltree interval lt2) l2 p2 pt2
      else make ltree l p rtree  

let split n tree =
  let exists = mem n tree in
    let big = 
      let rec search drzewo =
        match drzewo with
	| Empty -> Empty
	| Node (ltree, l, p, rtree, _, _) ->
           if n < l then join (search ltree) (l, p) rtree
           else search rtree
           (* wewnatrz nie ma bo usunelismy element n *)
      in search (remove (n, n) tree) 
    and 
    small =
      let rec search drzewo =
        match drzewo with
	| Empty -> Empty 
	| Node (ltree, l, p, rtree, _, _) ->
            if n < l then search ltree
            else join ltree (l, p) (search rtree)
            (* wewnatrz nie ma bo usunelismy element n *)
      in search (remove (n, n) tree)
    in (small, exists, big)
        
let below n tree = (* liczba elementow <= n , dodawanie z uwzgl. zakresu inta *)
  let rec search suma drzewo =
      match drzewo with
      | Node (ltree, l, p, rtree, _, sub) ->
        if n >= l && n <= p then 
          let len = n - l + 1 in let wew = if len > 0 then len else max_int in
          dod4 suma (pod ltree) (dl ltree) wew 
        else if n < l then search suma ltree
        else search (dod4 suma (pod ltree) (dl ltree) (dl drzewo)) rtree
      | Empty -> suma
  in search 0 tree
  
let fold f tree acc =
  let rec loop acc = function
    | Empty -> acc
    | Node (ltree, l, p, rtree, _, _) -> 
        loop (f (l, p) (loop acc ltree)) rtree in
  loop acc tree

let iter f tree =
  let rec loop = function
    | Empty -> ()
    | Node (ltree, l, p, rtree, _, _) -> 
        loop ltree; f (l, p) ; loop rtree in
  loop tree

let elements tree = (* posortowana lista przedzialow *)
  List.rev (fold (fun interval acc -> interval::acc) tree [])
 
(* ========================================================================= *)
(*                               TESTY                                       *)
(* ========================================================================= *)

(*

let a = empty
let a = add (17, 20) a
let a = add (5, 8) a
let a = add (1, 2) a
let a = add (10, 12) a
let a = add (28, 35) a
let a = add (22, 23) a
let a = add (40, 43) a
let a = add (37, 37) a;;

assert (is_avl a);;
 
let a = empty;;      
let a = remove (38, 80) a 
let a = remove (0, 75) a
let a = remove (95, 98) a
let a = add (28, 88) a
let a = add (67, 97) a
let a = add (60, 90) a
let a = remove (43, 74) a 
let a = add (1, 96) a
let a = remove (8, 23) a 
let a = remove (11, 58) a
let a = add (25, 96) a
let a = remove (5, 48) a 
let a = add (6, 73) a 
let a = remove (36, 95) a 
let a = add (13, 78) a
let a = add (23, 27) a
let a = add (68, 78) a
let a = remove (13, 76) a
let a = remove (43, 83) a
let a = add (60, 63) a
let a = remove (27, 87) a 
let a = add (73, 76) a
let a = remove (33, 74) a 
let a = add (44, 49) a
let a = remove (8, 11) a 
let a = add (27, 82) a
let a = remove (55, 78) a 
let a = add (51, 70) a
let a = remove (83, 96) a
let a = remove (74, 75) a
let a = add (0, 2) a
let a = remove (66, 76) a
let a = remove (59, 72) a
let a = remove (43, 47) a;;

assert (is_avl a);;

*)
(*
let a = empty;;
let a = add (0, 0) a;;
let a = add (max_int, max_int) a;;

let dodaj a =
  let rec search d i =
    if i > 30 then d
    else search (add (i, i + 9) d) (i + 20)
  in search a 0

let a = dodaj a;;
elements a;;

let s = add (min_int + 4, -7) empty;;
let s = add (8, max_int - 3) s;;
below (max_int - 5) s;;
*)
