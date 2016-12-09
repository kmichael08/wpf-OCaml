(* potomek *)
type 'a treef = Nil | Node of 'a treef * 'a * 'a treef ref * 'a treef

let potomek d = 
  let rec dfs t =
  match t with
  | Nil -> (0, Nil)
  | Node (l, _, refer, p) ->
      let (lg, lr) = dfs l and (pg, pr) = dfs p in
        let (mg, mr) = if lg < pg then (pg + 1, pr)
                       else (lg + 1, lr)
        in
          refer := mr;
          if mr = Nil then refer := t;
          if mr = Nil then (mg, t) else (mg, mr)
  in let _ = dfs d in ()

let a = Node (Nil, 6, ref Nil, Nil)
let b = Node (Nil, 5, ref Nil, Nil)
let c = Node (Nil, 3, ref Nil, Nil)
let d = Node (a, 4, ref Nil, Nil)
let e = Node (d, 2, ref Nil, b)
let f = Node (e, 1, ref Nil, c)

let wart tree =
  match tree with
  | Nil -> failwith "Pusta"
  | Node (_, w, _, _) -> w

let referencja tree =
  match tree with
  | Nil -> failwith "Pusta"
  | Node (_, _, r, _) -> !r

let pot wezel = 
  wart (referencja wezel)

let lista d =
  let rec przejscie acc t =
    match t with
    | Nil -> acc
    | Node (l, _, _, r) -> (przejscie [] l) @ (przejscie ((wart t, pot t)::acc) r)
  in przejscie [] d
