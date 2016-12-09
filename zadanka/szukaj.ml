(* szukaj x y - mamy tablice znakow x i tablice znakow y.
szukamy najdluzszego prefiksu y, ktory jest spojnym fragmentem x *)
(* sklejamy y i x, dajac pomiedzy jakas wartosc, ktora nie wystepuje tam 
i bierzemy max z tablicy prefiksowej *)

let maks = (* tablica nieujemna *)
  Array.fold_left (fun res el -> max res el) 0 

let szukaj x y = 
  let p = Kmp.pref (Array.append (Array.append y [|'#'|]) x) in
    maks p

let x = [|'k'; 't'; 'd'; 'e'; 'a'; 'c'; 'a'; 'b'; 'b'; 'a'; 'r'; 'e'; 'a'; 'b'; 'b'; 'f'|];;
let y = [|'a'; 'b'; 'b'; 'a'; 'd'; 'e'|];;
