type 'a tree = Node of 'a tree * 'a * 'a tree | Leaf

let rec fold_tree f a t =
    match t with
    |Leaf -> a
    |Node (l, x, r) -> f x (fold_tree f a l) (fold_tree f a r)
   
exception NieAVL

let avl t =
    let pom _ lw rw = if abs (lw - rw) <= 1 then max lw rw + 1 else raise NieAVL
    in try fold_tree pom 0 t; true with NieAVL -> false
