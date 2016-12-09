(* zadanie segment 13/14 *)
(* chcemy znalezc (w n log n) najdluzszy fragment spojny, w ktorym min jest wieksze od dlugosci*)


let segment tab =
  let n = Array.length tab in
    let tablica = Array.init n (fun i -> (tab.(i), i)) in
    Array.sort (fun (el, _) (el2, _) -> el2 - el) tablica;
    (* mamy tablice posortowana malejaco i indeksy trzymamy *)
    (* teraz idziemy od najwiekszego i tworzymy zbiory,
       trzymamy maksimum dlugosci, czyli potrzebujemy trzymac 
     *)
    let lewy = Array.make n (-1) and
    prawy = Array.make n (-1) in
      let max_dl = ref 0 and maks = ref 0 in
        for i = 0 to n - 1 do 
          let (wart, indeks) = tablica.(i) in
            lewy.(indeks) <- indeks;
            prawy.(indeks) <- indeks;
            if indeks >= 1 && lewy.(indeks - 1) <> -1 then
              begin
                let l = lewy.(indeks - 1) in
                  lewy.(indeks) <- l;
                  prawy.(l) <- prawy.(indeks)
              end;
            if indeks + 1 < n && prawy.(indeks + 1) <> -1 then
              begin
                let p = prawy.(indeks + 1) in
                  prawy.(indeks) <- p;
                  lewy.(p) <- lewy.(indeks);
                  prawy.(lewy.(indeks)) <- p
              end;
            max_dl := max !max_dl (prawy.(indeks) - lewy.(indeks) + 1);
            if wart >= !max_dl then maks := max !maks !max_dl
            else maks := max !maks wart
        done;
        !maks
        

(* Nie dziala !!! *)


let tab = [|6; 2; 8; 1; 7; 3; 5|];;
let tab2 = [|4; 9; 100; 8; 11; 7; 9|];;
