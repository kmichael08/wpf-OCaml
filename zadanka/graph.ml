

module type GRAPH =
sig
(* Etykiety krawędzi. *)
type graph
(* Pusty graf. *)
val init : int ->graph
(* Rozmiar grafu *)
val size : graph ->int
(* Dodanie krawędzi skierowanej łączącej dwa wierzchołki. *)
val insert_directed_edge : graph ->int ->int -> int ->unit
(* Dodanie krawędzi nieskierowanej (dwukierunkowej) łączącej dwa wierzchołki. *)
val insert_edge : graph ->int ->int ->int ->unit
(* Lista incydencji danego wierzchołka. *)
val neighbours : graph ->int ->(int* int) list
end;;


module Graph =
(struct
(* Etykiety krawędzi. *)
type graph = {n : int; e : (int*int) list array}
(* Pusty graf. *)
let init s = {n = s; e = Array.make s []}
(* Rozmiar grafu. *)
let size g = g.n
(* Dodanie krawędzi skierowanej łączącej dwa wierzchołki. *)
let insert_directed_edge g x y l =
assert ((x<>y) && (x >= 0) && (x <size g) && (y >= 0) && (y <size g) &&
(List.filter (fun (v,_) ->v=y) g.e.(x) = []));
g.e.(x) <- (y,l)::g.e.(x)
(* Dodanie krawędzi łączącej dwa (istniejące) wierzchołki. *)
let insert_edge g x y l =
insert_directed_edge g x y l;
insert_directed_edge g y x l
(* Lista incydencji danego wierzchołka. *)
let neighbours g x =
g.e.(x)
end);;

open Graph;;
let a = init 10;;
let a = insert_edge a 1 2 5;;
let a = insert_edge a 1 3 4;;
let a = insert_edge a 2 7 100;;
