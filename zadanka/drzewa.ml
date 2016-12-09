open List;;

type 'a drzewo =  Node of 'a * 'a drzewo list;;

let rec fold_tree f (Node (x,l)) =
f x (map (fold_tree f) l);;

let a = Node(5, []);;
let b = Node(6, []);;
let c = Node(2, [a; b]);;
let d = Node(7, []);;
let e = Node(8, []);;
let f = Node(9, []);;
let g = Node(11, []);;
let h = Node(10, [g]);;
let i = Node(4, [e; f; h]);;
let j = Node(3, [d]);;
let k = Node(1, [c; j; i]);;

(* maximum na liscie *)
let height tree =
  let maxl lista = fold_left max 0 lista in
  fold_tree (fun w lista -> maxl lista + 1 ) tree;;

let map_tree f t =
fold_tree (fun x l ->Node (f x,l)) t;;

