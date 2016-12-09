(* ==================================================================== *)
(*  Zadanie przelewanka, Autor : Michał Kuźba, Reviewer : Paweł Zięcik  *)
(* ==================================================================== *)

let warunek tab = 
  Array.fold_left (fun logic (x, y) -> logic || y = 0 || x = y) false tab

(* sprawdzamy warunek konieczny - czy na koncu ktoras szklanka ma byc
pusta lub pelna, w ostatnim ruchu oproznimy jakas szklanka lub ja napelnimy *)

let rec gcd a b = (* nwd dwoch liczb *)
  if b = 0 then a else
    gcd b (a mod b)

let nwd tab = (* nwd tablicy *)
  Array.fold_left (fun d (el, _) -> gcd d el) (fst tab.(0)) tab
 
let podzielne tab d = 
  let predykat y = if d = 0 then y = 0 else y mod d = 0 in
    Array.fold_left (fun logic (_, y) -> logic && predykat y) true tab
 
(* czy wszystkie szklanki na koncu sa podzielne przez nwd pojemnosci szklanek,
wiemy ze w kazdym momencie ilosc wody w szklance jest podzielna przez to nwd *)

let ruch glass uklad x y =
  let n = Array.length glass in 
    let vol = if y <> n then fst glass.(y) else 0 in
      if x = y || (x <> n && uklad.(x) = 0) || (y <> n && uklad.(y) = vol)
        then [||]  (* ruch nic nie zmienia *)
      else 
        begin
          if x = n then uklad.(y) <- vol (* nalewamy z kranu *)
          else if y = n then uklad.(x) <- 0 (* wylewamy do zlewu *)  
          else 
            begin
              let dif = vol - uklad.(y) in
                if uklad.(x) <= dif then 
                  begin 
                    uklad.(y) <- uklad.(y) + uklad.(x); 
                    uklad.(x) <- 0 
                  end 
                else
                  begin 
                    uklad.(x) <- uklad.(x) - dif; 
                    uklad.(y) <- vol
                  end         
            end;
            uklad
        end

(* zwraca uklad po przelaniu wody z szklanki x do y. Jesli to nic nie zmienia
zwraca pusta konfiguracje. Zlew liczymy jako dodatkowo szklanke z indeksem n *) 

let przelewanka glass =
  if glass = [||] then 0 else 
  let n = Array.length glass in
    if not (warunek glass) then -1 
    else if not (podzielne glass (nwd glass)) then -1 
    else
      let kol = Queue.create()  
      and pocz = Array.make n 0  (* konfig. poczatkowa *)
      and kon = Array.init n (fun i -> snd glass.(i)) (* konfig. koncowa *)
      and tbl = Hashtbl.create 19 and ile = ref 0 
      and solution = ref false in         
        Queue.add (pocz, 0) kol;
        Hashtbl.add tbl pocz 0 ;
        while not (Queue.is_empty kol) && not !solution do
          let (state, nr) = Queue.pop kol in
            ile := nr;
            if state = kon then solution := true else
              for i = 0 to n do
                if i = n || state.(i) <> 0 then
                  for j = 0 to n do
                    let stan = ruch glass (Array.copy state) i j in
                      if stan <> [||] && not (Hashtbl.mem tbl stan) then
                        begin
                          Queue.add (stan, nr + 1) kol;
                          Hashtbl.add tbl stan 0
                        end
                  done
              done           
        done;
        if !solution then !ile else -1  
    
(* 
przechodzimy przez mozliwe stany bfs-em, wrzucamy mozliwy stan na kolejke
sprawdzajac na hash tablicy czy nie byl juz wrzucony. Potem wrzucamy wszystkie 
mozliwe uklady po wykonaniu jednej operacji. Jesli dojdziemy do koncowego 
stanu to znalezlismy liczbe operacji (minimalna, bo przechodzimy bfsem)
jesli wyczerpiemy wszystkie mozliwe stany nie znajdujac rozwiazania
to zwracamy -1. Rozmiar hashtablicy jest zmienny wiec dany rozmiar jest tylko
poczatkowy.
*) 
