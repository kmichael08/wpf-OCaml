(*         Zadanie : Sortowanie Topologiczne           *)
(* Autor : Michał Kuźba , Reviewer : Dominika Bakalarz *)

exception Cykliczne
 
let mapa ll = (* mapa - klucz wierzcholek, wartosc lista sasiadow *)
  List.fold_left (fun m (w, l) -> PMap.add w l m) (PMap.empty) ll 

let topol ll =
  let m = mapa ll and vis = ref PMap.empty in
    let sorted =
      PMap.foldi 
        (fun h _ result ->
          if not (PMap.mem h !vis) then
            begin
              let rec dfs w =
                vis := PMap.add w 1 !vis; (* przetwarzany *)
                List.iter (fun el ->
                             if not (PMap.mem el !vis) then dfs el
                             else if PMap.find el !vis = 1 then raise Cykliczne 
                          )
                (try PMap.find w m with Not_found -> []);             
                result := w::(!result);   
                vis := PMap.add w 2 !vis (* przetworzony *)    
              in dfs h
            end;
            result
       )
       m (ref [])
    in !sorted

(* Przechodzimy graf dfsem, brak wartosci w vis oznacza wierzcholek 
  nieodwiedzony, 1 oznacza wierzcholek, ktorego przetwarzanie sie nie 
  zakonczylo, a 2 calkowicie przetworzony, jesli dojdziemy do wierzcholka
  przetwarzanego to znalezlismy cykl; po przetworzeniu 
  dodajemy wierzcholek
  na stos (liste) *)
