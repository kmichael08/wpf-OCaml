module type GRAPH =
sig
(* Etykiety krawędzi. *)
type graph
(* Pusty graf. *)
val init : int -> graph
(* Rozmiar grafu *)
val size : graph ->int
(* Dodanie krawędzi skierowanej łączącej dwa wierzchołki. *)
val insert_directed_edge : graph -> int -> int -> unit
val insert_edge : graph ->int ->int ->unit
(* Lista incydencji danego wierzchołka. *)
val neighbours : graph ->int ->int list
end;;

module Graph =
struct
(* Typ grafów - tablica list sąsiedztwa. *)
type graph = {n : int; e : int list array}
(* Pusty graf. *)
let init s = {n = s; e = Array.make s []}
(* Rozmiar grafu. *)
let size g = g.n
(* Dodanie krawędzi skierowanej łączącej dwa wierzchołki. *)
let insert_directed_edge g x y =
assert ((x<>y) && (x >= 0) && (x <size g) && (y >= 0) && (y <size g) &&
(List.filter (fun v -> v=y ) g.e.(x) = []));
g.e.(x) <- y::g.e.(x)
(* Dodanie krawędzi łączącej dwa (istniejące) wierzchołki. *)
let insert_edge g x y =
insert_directed_edge g x y;
insert_directed_edge g y x
(* Lista incydencji danego wierzchołka. *)
let neighbours g x =
g.e.(x)
end;; 

open Graph;;

let g = init 8;;

insert_edge g 0 5;;
insert_edge g 5 1;;
insert_edge g 1 7;;
insert_edge g 4 6;;
insert_edge g 0 7;;
insert_edge g 2 4;;
insert_edge g 2 0;;
insert_edge g 4 5;;
insert_edge g 5 3;;
insert_edge g 3 1;;

g;;

let path g = 
  let n = size g in
    let tab = Array.make n 0 in
      for i = 0 to n - 1 do
        List.iter (fun el -> if el > i then tab.(el) <- max tab.(el) (tab.(i) + 1)) (neighbours g i)
      done;
      let maks = ref 0 in
        for i = 0 to n - 1 do
          if tab.(i) > !maks then maks := tab.(i)
        done;
        !maks
