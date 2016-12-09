open List;;

type 'a tree = Nil | Node of 'a tree * 'a * 'a tree

let rec fold_tree f a d = 
  match d with
  | Nil -> a
  | Node (l, w, p) -> f w (fold_tree f a l) (fold_tree f a p)

(* infiksowy porzadek *)
let infiks d = fold_tree (fun x fl fr y -> fl (x::fr y))
                         (fun x -> x) d []

(* (fun x -> x) to wartosc dla pustego, [] podstawimy na koniec  *)

let levels d = 
  fold_tree (fun x fl fr l -> match l with
			     | [] -> [x]::fl (fr [])
			     | h::t -> (x::h)::fl (fr t))
            (fun x -> x) d []



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


let skosokosc t =
  let rec skos liczba d =
    match d with
    | Node (l, v, p) -> if 2 * skos l > 2 * skos p + 1 then skos liczba
