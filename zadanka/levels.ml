open List;;

type 'a tree = Node of 'a * 'a tree list;;
 
let rec fold_tree f (Node (x,l)) =
  f x (map (fold_tree f) l);;

let merge l1 l2 =
  let rec pom wynik l1 l2 =
    match l1, l2 with
    | [], [] -> wynik
    | h::t, g::o -> pom ((h + g)::wynik) t o
    | [], g::o -> pom (g::wynik) [] o
    | h::t, [] -> pom (h::wynik) t []
  in rev (pom [] l1 l2)

let polacz ll = fold_left (fun result l -> merge result l) [] ll;;


let a = Node (9, []);;
let b = Node (8, []);;
let c = Node (7, [b; a]);;
let d = Node (6, [c]);;
let e = Node (5, []);;
let f = Node (2, [e; d]);;
let g = Node (3, []);;
let h = Node (4, []);;
let i = Node (1, [f;g;h]);;

let levels tree = fold_tree (fun x lista -> if lista = [] then [1]  
                                            else (0::(polacz lista) ))
                             tree;;

levels i;;

let maxl l = fold_left max 0 l;;

let depth tree = fold_tree (fun x lista -> if lista = [] then 0 
                                           else maxl lista + 1) tree;;

let maxwidth tree = fold_tree (fun x lista -> if lista = [] then 1
                                         else max (maxl lista) (length lista))
               tree;;

maxwidth i;;
maxwidth d;;
