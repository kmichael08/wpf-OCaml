(* zadanie szablon - chcemy znalezc najkrotszy szablon spelniajacy warunki
-pokryc nim mozna caly tekst
-nie ma dziur
-moze wielokrotnie, ale nie nachodza na siebie rozne litery
-uzywamy calego szablonu *)
(* duzo roznych rozwiazan *)

#load "kmp.cmo";;
(* szablon jest prefiksem i sufiksem, stad wystarczy znalezc wszystkie
prefikso - sufiksy i je sprawdzic *)
(* napisac funkcje sprawdzajaca czy taki prefiks zadziala *)
(* napisac funkcje tworzaca ta liste *)
(* jak przejsc dobrze po tej liscie *)

let check wzorzec slowo =
  let wyst = Kmp.find wzorzec slowo 
  and n = Array.length wzorzec in
   fst( List.fold_left (fun (logic, prev) el -> if el - prev > n then 
                          (false, el) else (logic, el))
                   (true, List.hd wyst) (List.tl wyst))
(*
let slowo = [|1; 2; 2; 1; 2; 2; 1; 2; 2; 1; 2; 2; 1; 2; 2; 1; 1; 2; 2; 1|];;
check [|1; 2|] slowo;;
check [|1; 2; 2|] slowo;;
check [|1; 2; 2; 1|] slowo;;
 *)

(* tworzymy liste prefikso-sufiksow - zwraca ich dlugosci *)
let lista slowo = 
  let n = Array.length slowo and p = Kmp.pref slowo in
    let rec stworz i res =
      if p.(i) = 0 then res 
      else stworz (p.(i)) (p.(i)::res)
    in stworz (n) [n];;

let slowo = [|1; 2; 2; 1; 2; 2; 1; 2; 2; 1; 2; 2; 1; 2; 2; 1|];;
lista slowo;;

(* teraz przegladamy checkiem te liste od najkrotszych do najdluzszych *)

let brut slowo =
  let rec szukaj lis =
    match lis with
    | [] -> failwith "Blad!!! - przeciez cale slowo powinno zadzialac"
    | h::t -> let wzorzec = Array.sub slowo 0 h in 
                if check wzorzec slowo then wzorzec
                else szukaj t
  in szukaj (lista slowo);;
  
brut slowo;;

(* wzorcowe rozwiazanie nlogn przeglada tylko wzorce ponad 2 razy dlusze niz poprzednio *)
let szablon slowo = 
  let rec szukaj lis dlugosc =
    match lis with
    | [] -> failwith "Blad!!! przeciez cale slowo powinno zadzialac"
    | h::t -> if h <= 2 * dlugosc then szukaj t h
              else let wzorzec = Array.sub slowo 0 h in
                begin
                  if check wzorzec slowo then wzorzec
                  else szukaj t h
                end
  in szukaj (lista slowo) 0;;

szablon slowo;;

let slowo2 = [|1; 2; 3; 4; 5; 6|];;
let slowo3 = [|1; 1; 1|];;
let slowo4 = [|'a'; 'b'; 'a'; 'b'|];;
let slowo5 = [|1; 2; 1; 2; 2; 1; 2; 1; 2; 2; 1; 1; 2; 1; 2; 2; 1|];;

brut slowo2;;
szablon slowo2;;
brut slowo3;;
szablon slowo3;;
brut slowo4;;
szablon slowo4;;
brut slowo5;;
szablon slowo5;;
