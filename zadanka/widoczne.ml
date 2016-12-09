(* brut do zadania widoczne na zwyklych drzewach *)
open List;;

type 'a drzewo = Node of 'a * 'a drzewo list

let rec fold_tree f (Node (w, ld)) =
  f w (map (fold_tree f) ld)

let polacz ll = fold_left append [] ll;;

let wyrzuc x ll = x::(filter (fun y -> y >= x) (polacz ll))

let widoczne d = length (fold_tree (fun x l -> wyrzuc x l) d);;

let a = Node(12, []);;
let b = Node(7, [a]);;
let c = Node(5, []);;
let d = Node(10, []);;
let e = Node(8, [d;c;b]);;
let f = Node(4, []);;
let g = Node(6, [f; e]);;

