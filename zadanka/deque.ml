module type DeQueue =
sig
  type 'a dequeue
  val empty : 'a dequeue
  val is_empty : 'a dequeue -> bool
  val front : 'a dequeue -> 'a
  val back : 'a dequeue -> 'a
  val insert_front : 'a dequeue -> 'a -> 'a dequeue
  val insert_back : 'a dequeue -> 'a -> 'a dequeue
  val remove_front : 'a dequeue -> 'a dequeue
  val remove_back : 'a dequeue -> 'a dequeue
end;;

module Kol : DeQueue   with type 'a dequeue = 'a list * 'a list * int =
struct
  type 'a dequeue = 'a list * 'a list * int (* lista dolna, lista gorna i liczba elementow *)

  let empty = ([], [], 0)

  let is_empty (_, _, ile) = ile = 0

  exception Pusta;;

  let front q =
    match q with
    | (h::_, _, _) -> h
    | ([], [el], _) -> el
    | ([], [], _) -> raise Pusta;;
  
  let back q =
    match q with
    | (_, h::_, _) -> h
    | ([el], [], _) -> el
    | ([], [], _) -> raise Pusta;;

  let rec przepisz l k acc =
    if k = 0 then (List.rev (fst acc),List.rev(snd acc))
    else
    match l with 
    | []   -> failwith "Internal error - blad przepisywania"
    | h::t -> przepisz t (k-1) ((h::(fst acc)), t)     


  let balance q = (* balansowanie kolejki *)
  match q with
  |  ([], [], _)     -> q
  |  ([el], [], h)   -> q
  |  ([], [el], h)   -> q
  |  ([], l, n)      -> let (l1, l2) = przepisz l (n/2) ([], l) in (l2, l1, n)
  |  (l, [], n)      -> let (l1, l2) = przepisz l (n/2) ([], l) in (l1, l2, n)
  |  _               -> q

  let insert_front (f, b, n) el =
    balance (el::f, b, n+1)
  
  let insert_back (f, b, n) el =
    balance (f, el::b, n+1)

  let remove_front q =
    match q with
    | (_::f, b, n) -> balance (f, b, n-1)
    | (_, [el], _) -> empty
    | ([], [], 0)  -> raise Pusta;;

  let remove_back q =
    match q with
    | (f, _::b, n)  -> balance (f,b, n-1)
    | ([el], [], _) -> empty
    | ([], [], _)   -> raise Pusta;; 

  
end
