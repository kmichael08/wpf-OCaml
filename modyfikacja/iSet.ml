(* ========================================================================= *)
(*       Autor : Michał Kuźba,         Reviewer :                            *)
(* ========================================================================= *)

type  t = Empty | Node of  t * int * int * t * int * int
(* lewe drzewo, przedzial, prawe, wysokosc, ile "liczb" w poddrzewie lewym  *)

let wysokosc tree = 
  match tree with
  | Node (_, _, _, _, h, _) -> h
  | Empty                   -> 0

let pod tree =
  match tree with
  | Node (_, _, _, _, _, sub) -> sub
  | Empty                     -> 0

let dl tree =
  match tree with
  | Node (_, l, p, _, _, _) -> p - l
  | Empty                   -> 0

let empty = Empty

let is_empty tree = tree = Empty

let rec interval el tree = (* przedzial zawierajacy el *)
  match tree with
  | Empty -> (el + 1, el - 1) (* nie zawiera *)
  | Node (ltree, l, p, rtree, _, _) ->
      if el >= l && el <= p then (l, p)
      else if el < l then interval el ltree
      else interval el rtree

let mem el tree = (* czy el jest w drzewie *)
  interval el tree <> (el + 1, el - 1)

let smallest el tree = (* najm. element z przedzialu zawierajacego el *)
  assert (interval el tree <> (el + 1, el - 1));
  fst (interval el tree)

let largest el tree =  (* najw. element z przedzialu zawierajacego el *)
    assert (interval el tree <> (el + 1, el - 1));
    snd (interval el tree)

let rec remp drzewo left right = (* wycinamy z drzewa przedzial [left, right] *)
  match drzewo with
  | Empty -> Empty
  | Node (ld, ll, pp, pd, h, s) ->
      if right < ll then let nowy = remp ld left right in
        Node (nowy, ll, pp, pd, (max (wysokosc nowy) (wysokosc pd)) + 1, s)
      else if right < pp then Node (Empty, right + 1, pp, pd, wysokosc pd + 1, s)
      else if right = pp then pd
      else remp pd left right

let rec reml drzewo left right = (* wycinamy z drzewa przedzial [left, right] *)
  match drzewo with
  | Empty -> Empty
  | Node (ld, ll, pp, pd, h, s) ->
      if left > pp then let nowy = reml pd left right in
        Node (ld, ll, pp, nowy, (max (wysokosc ld) (wysokosc nowy)) + 1, s)       
      else if left > ll then Node (ld, ll, left - 1, Empty, wysokosc ld, s)
      else if left = ll then ld 
      else reml ld left right


let make ltree l p rtree =
   Node (ltree, l, p, rtree, max (wysokosc ltree) (wysokosc rtree) + 1, p - l + 1 + pod ltree )




let rec add lewy prawy t = 
  if lewy > prawy then t
  else
  match t with
  | Empty -> Node (Empty, lewy, prawy, Empty, 1, 0)
  | Node (ltree, l, p, rtree, height, sub) ->
      if lewy > p + 1 then let nowy = add lewy prawy rtree in
        Node (ltree, l, p, nowy, max height (wysokosc nowy + 1), sub)

      else if prawy < l - 1 then let nowy = add lewy prawy ltree in
        Node (nowy, l, p, rtree, max height (wysokosc nowy + 1), sub + prawy - lewy)
     
      else if lewy >= l && prawy <= p then t
      
      else if lewy = p + 1 then 
        begin
          if mem (prawy + 1) t then
            let limit = largest (prawy + 1) t in
              let nowy = remp rtree lewy limit in
                Node (ltree, l, limit, nowy, max (wysokosc ltree) (wysokosc nowy) + 1, sub)
          else let nowy = remp rtree lewy prawy in
            Node (ltree, l, prawy, nowy , max (wysokosc ltree) (wysokosc nowy) + 1, sub)
        end
          
      else if prawy = l - 1 then
        begin
          if mem (lewy - 1) t then
            let limit = smallest (lewy - 1) t in
              let nowy = reml ltree limit prawy in
                Node (nowy, limit, p, rtree, max (wysokosc nowy) (wysokosc rtree) + 1, sub)
          else let nowy = reml ltree lewy prawy in
            Node (nowy, lewy, p, rtree, max (wysokosc nowy) (wysokosc rtree) + 1, sub)
        end 
      
      else if lewy >= l then add (p + 1) prawy t
      else if prawy <= p then add lewy (l - 1) t
      else let pierwszy = add lewy (l - 1) t 
      in add (p + 1) prawy pierwszy
      


(* jakims foldem moze (wlasnym ?) !!!, tymczasem wkazuja tez wysokosc *)
let elements tree = 
  let rec przedzialy acc drzewo = 
    match drzewo with
    | Empty -> acc
    | Node (ltree, l, p, rtree, h, _) ->
        przedzialy ((l, p, h) :: przedzialy acc rtree) ltree    
  in przedzialy [] tree


let below n tree = (* liczba elementow <= n , pilnowac max_inta *)
  let rec search suma drzewo =
    match drzewo with
    | Empty -> suma
    | Node (ltree, l, p, rtree, _, sub) ->
        if n >= l && n <= p then suma + sub + (n - l + 1)
        else if n < l then search suma ltree
        else search (suma + sub + p - l + 1) rtree
  in search 0 tree
  
let fold f tree acc =
  let rec loop acc = function
    | Empty -> acc
    | Node (ltree, l, p, rtree, _, _) -> 
        loop (f l p (loop acc ltree)) rtree in
  loop acc tree

let iter f tree =
  let rec loop = function
    | Empty -> ()
    | Node (ltree, l, p, rtree, _, _) -> 
        loop ltree; f l p ; loop rtree in
  loop tree
        
