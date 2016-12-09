open List;;
type 'a tree = Node of 'a tree * 'a * 'a tree | Leaf;;

let construct_tree pre inf =
    if pre = [] then Leaf
    else let rec get_tree pre inf stop =
        let me = hd pre
        in let (pre, inf, ltree) =
            if hd inf = me then (tl pre, inf, Leaf)
            else get_tree (tl pre) inf me
        in let inf = tl inf
        in let (pre, inf, rtree) =
            if inf = [] || hd inf = stop then (pre, inf, Leaf)
            else get_tree pre inf stop
        in (pre, inf, Node(ltree, me, rtree))
    in match get_tree pre inf (hd pre) with
    | (_, _, result) -> result;;

