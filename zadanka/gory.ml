module type PRI_QUEUE = sig
type 'a pri_queue
val empty_queue : 'a pri_queue
val is_empty : 'a pri_queue -> bool
val put : 'a pri_queue -> 'a -> 'a -> 'a -> int -> 'a pri_queue
val getmin : 'a pri_queue -> 'a
val removemin : 'a pri_queue -> 'a pri_queue
exception Empty_Queue
end;;

module PRI_QUEUE = struct
type 'a pri_queue = 'a list

let empty_queue = []

let is_empty q = q = []

let put a q comp = 
  let rec dodaj lewa prawa =
    if prawa = [] || comp a (List.hd prawa) <= 0 then (List.rev lewa) @ [a] @ prawa
    else dodaj ((List.hd prawa)::lewa) (List.tl prawa)
  in dodaj [] q

let getmin q = List.hd q

exception Empty_Queue

let removemin q = match q with
  | []   -> raise Empty_Queue
  | _::t -> t

end

(* Mamy juz zaimplementowana parakolejke priorytetowa, funkcyjna *)



let m1 = [|3; 17; 11; 9; 2|];;
let m2 = [|20; 8; 4; 13; 7|];;
let m3 = [|10; 11; 12; 8; 5|];;
let m4 = [|7; 3; 9; 4; 14|];;

let mapa = [|m1; m2; m3; m4|];;

let mapa2 = [|m4; m3; m2; m1|];;

let min_wysokosc mapa =
  let comp (x, y) (a, b) = compare y b in                           (* funkcja do wrzucania po priorytecie na wartosc *)
  let n = Array.length mapa and m = Array.length mapa.(0) in
    let vis = Array.make_matrix n m false
    and ruch x y = [(x + 1, y); (x - 1, y); (x, y + 1); (x, y - 1)] (* lista mozliwych ruchow *)
    and wynik = ref 0 
    and kol = ref empty_queue in
      let czy_dobry x y =                                           (* czy mozemy isc na dana pozycje *)
        (x >= 0 && y >= 0 && x < n && y < m && not vis.(x).(y)) in
        kol := put ((0, 0), mapa.(0).(0)) !kol comp;                (* zaczynam od (0, 0) *)

        while fst (getmin !kol) <> (n - 1, m - 1) do
          let ((x, y), wart) = getmin !kol in
          (*Printf.printf "%d \n" wart; *)
          wynik := max !wynik (snd (getmin !kol));
          kol := removemin !kol;
          if not vis.(x).(y) then
            begin
              vis.(x).(y) <- true;
              List.iter (fun (a, b) -> if czy_dobry a b then kol := put ((a, b), mapa.(a).(b)) !kol comp) (ruch x y) (* wszystkie mozliwe ruchy *)
            end
       done;
       !wynik
       
          
  
