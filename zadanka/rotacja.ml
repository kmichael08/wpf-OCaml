#load "kmp.cmo"
(* zadanie rotacja - znalezc takie nietrywialne rotacje listy o k,
ze sa identyczne z poczatkowa lista 
latwiej dzialac na tablicy, bo tak dziala nasz kmp
*)

let rotacja lista =
  let wzorzec = Array.of_list lista in
    let slowo = Array.append wzorzec wzorzec 
      and n = List.length lista in 
        List.filter (fun el -> el > 0 && el < n) (Kmp.find wzorzec slowo)

let lista = [0; 1; 0; 1; 0; 1];;

rotacja lista;;
