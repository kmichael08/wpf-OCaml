(* zadanie okresy *)
(* chcemy znalezc najmniejszy okres kazdego prefiksu slowa i zwrocic liste tych okresow
dla poszczegolnych prefiksow *)
(* obserwacja - najkrotszy okres to dlugosc listy - najdl. prefikso - sufiks *)
#load "kmp.cmo"
(* tablica pref - pref.(i) - najdluzszy prefikso - sufiks slowa konczacego sie w i, czyli
jest to tablica najdluszych prefikso - sufiksow kazdego prefiksu *)

let okresy znaki = 
  let tab = Array.of_list znaki in
    let p = Kmp.pref tab and n = List.length znaki in
      let rec stworz i res =
        if i = n then res
        else stworz (i + 1) ((i - p.(i))::res)
      in stworz 0 []

let znaki = [1; 2; 3; 5; 1; 2; 3; 5; 1; 2; 3; 5];;

okresy znaki;;
