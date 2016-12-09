open List;;

type 'a drzewo = Node of 'a * 'a drzewo list

let rec fold_tree f (Node (w, ld)) =
  f w (map (fold_tree f) ld)

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


let maxl l = fold_left max 0 l;;

let depth t =
  fold_tree (fun _ l -> maxl l + 1) t;;

let maksiu l = fold_left (

let srednica t =
  fold_tree (fun _ l -> maksiu l) t;;
