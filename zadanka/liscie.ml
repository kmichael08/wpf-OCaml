open List;;

(* brut do zadanka liscie *)

type 'a tree = Node of 'a * 'a tree list;;

let rec fold_tree f (Node (w, ld)) =
  f w (map (fold_tree f) ld)

let join l1 l2 = 
  let rec pom wynik lista1 lista2 =
    match lista1, lista2 with
    | h::t, g::o -> pom ((h + g):: wynik) t o
    | [], []     -> wynik
    | h::t, []   -> pom (h::wynik) t []
    | [], g::o    -> pom (g::wynik) [] o
  in rev (pom [] l1 l2)

let l1 = [2; 4; 1; 1];;
let l2 = [3; 5];;
let l3 = [0; 1; 7];;

let lista = [l1; l2; l3];;


let merge ll = 0::(fold_left join [] ll)

merge lista;;

let liscie d = fold_tree (fun _ l -> if l =[] then [1] else merge l) d

let a = Node (13, []);;
let b = Node (11, [a]);;
let c = Node (4, [b]);;
let d = Node (12, []);;
let e = Node (10, [d]);;
let f = Node (9, []);;
let g = Node (8, []);;
let h = Node (5, [g; f; e]);;
let i = Node (6, []);;
let j = Node (2, [c; h; i]);;
let k = Node (3, []);;
let m = Node (1, [j; k]);;
