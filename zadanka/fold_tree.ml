open List;;

(* drzewo binarne *)
type 'a tree = Nil | Node of 'a tree * 'a * 'a tree

(* zwykle drzewo 
type 'a drzewo = Node of 'a * 'a drzewo list
 *)

(* fold_tree na binarnych *)
let rec fold_tree f a d =
  match d with
  | Nil            -> a
  | Node (l, w, p) -> f w (fold_tree f a l) (fold_tree f a p)

(* fold _tree na zwyklych *)
(*
let rec fold_tree f (Node (w, ld)) =
  f w (map (fold_tree f) ld)
 *)

(* liczba wezlow w drzewie *)
let knots d = fold_tree (fun _ lewe prawe -> lewe + prawe + 1) 0 d;;

let a = Node (Nil, 12, Nil);;
let b = Node (Nil, 11, Nil);;
let c = Node (b, 9, a);;
let d = Node (Nil, 8, Nil);;
let e = Node (d, 7, c);;
let f = Node (Nil, 6, Nil);;
let g = Node (f, 3, e);;
let h = Node (Nil, 10, Nil);;
let i = Node (h, 5, Nil);;
let j = Node (Nil, 4, Nil);;
let k = Node (j, 2, i);;
let m = Node (k, 1, g);;

(* wysokosc drzewa *)
let height d = fold_tree (fun _ lewe prawe -> max lewe prawe + 1) 0 d;;

let max3 a b c = max a (max b c)
let min3 a b c = min a (min b c)

(* srednica drzewa *)
let srednica d = snd (
  fold_tree (fun _ (lewagl, lres) (prawagl, pres) ->
                       (max lewagl prawagl + 1, max3 (lewagl + prawagl + 2) lres pres))
            (-1, 0) d)

(* czy drzewo jest avl - dla kazdego wezla glebokosci prawego i lewego roznia sie o najwyzej 1 *)
let avl d = snd (
  fold_tree (fun _ (lgl, blewe) (pgl, bprawe) ->
            (max lgl pgl + 1 , bprawe && blewe && (abs (lgl - pgl) <= 1)) )
            (0, true) d )

(* czy drzewo jest typu bst na lewo mniejsze od wierzcholka a na prawo wieksze *)
let bst d = let _, _, logic =
  fold_tree (fun w (minl, maxl, bstl) (minp, maxp, bstp) ->
            (min3 w minl minp, max3 w maxl maxp, bstl && bstp && w > maxl && w < minp))
            (max_int , 0, true) d
            in logic

let o = Node (Nil, 19, Nil)
let n = Node (Nil, 13, Nil)
let p = Node (n, 17, o)
let q = Node (Nil, 12, Nil)
let r = Node (q, 14, p)
let w = Node (Nil, 2, Nil)
let u = Node (w, 6, Nil)
let v = Node (Nil, 9, Nil)
let t = Node (u, 8, v)
let s = Node (t, 10, r)
