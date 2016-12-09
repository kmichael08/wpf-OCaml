type tree = Node of tree * tree | Leaf

let rec fold_tree f a t =
    match t with
    |Leaf -> a
    |Node (l, r) -> f (fold_tree f a l) (fold_tree f a r)

let rec skosnosc t = match t with
    |Leaf -> 0
    |Node(l, r) -> max (2 * skosnosc l) (2 * skosnosc r + 1)

let rec skosokosc t =
    let f (l, sl) (r, sr) = if sl >= sr then (Node(l, r), max (2 * sl) (2 * sr + 1))
    else  (Node(r, l), max (2 * sl +1) (2 * sr)) in
    fst (fold_tree f (Leaf, 0) t)
