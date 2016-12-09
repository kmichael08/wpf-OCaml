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

(* musimy znalezc wszystkie palindromy parzyste w O(n^2) *)
let utworz l = 
  let n = List.length l in
    let tab = Array.make n (List.hd l) in
      let nr = ref 0 in
        List.iter (fun el -> tab.(!nr) <- el; incr nr) l;
        let g = Graph.init (n + 1) in
          for i = 0 to n - 2 do
            let j = ref 0 in
              while i - !j >= 0 && i + !j + 1 < n && tab.(i - !j) = tab.(i + !j + 1) do
                Graph.insert_directed_edge g (i - !j) (i + !j + 2);
                incr j;
              done
          done;
          g
                
let l = ['a'; 'b'; 'b'; 'a'];;

let lista = ['a'; 'b'; 'b'; 'a'; 'a'; 'b'; 'b'; 'a'; 'c'; 'c'; 'a'; 'b'; 'b'; 'a'];;

let podzial l = 
  let g = utworz l in 
    let n = List.length l in
      let ilosc = Array.make (n + 1) (n + 2) in
        ilosc.(0) <- 0;
        for i = 0 to n - 1 do
          List.iter (fun el -> ilosc.(el) <- min ilosc.(el) (ilosc.(i) + 1)) 
                    (Graph.neighbours g i)
        done;
        let res = ilosc.(n) in if res > n then (-1) else ilosc.(n)
          
(* z odtworzeniem; wersja z bfsem *)
