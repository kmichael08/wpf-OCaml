let s1 = [|'a'; 'b'; 'b'; 'a'; 'c'; 'a'; 'd'; 'a'; 'b'|]
let s2 = [|'m'; 'a'; 'n'; 'k'; 'a'; 'c'; 'a'; 'p'; 'o'; 'r'; 't'; 'a'; 'b'|]

let nwp s1 s2 = 
  let n = Array.length s1 and m = Array.length s2 in
    let tab = Array.make_matrix (n + 1) (m + 1) 0 in
    for i = 1 to n do
      for j = 1 to m do
        if s1.(i - 1) = s2.(j - 1) then tab.(i).(j) <- tab.(i - 1).(j - 1) + 1
        else tab.(i).(j) <- max tab.(i - 1).(j) tab.(i).(j - 1)
      done
    done;
  tab

(* wersja ze zwracaniem wyniku *)
let podciag s1 s2 =
  let tab = nwp s1 s2 in
    let n = Array.length tab and m = Array.length tab.(0) in
      let rec odtworz x y slowo =
        if x = 0 || y = 0 then slowo
        else if tab.(x - 1).(y) = tab.(x).(y) then odtworz (x - 1) y slowo
        else if tab.(x).(y - 1) = tab.(x).(y) then odtworz x (y - 1) slowo
        else odtworz (x - 1) (y - 1) (s1.(x - 1)::slowo)
      in odtworz (n - 1) (m - 1) []

let sl1 = [|'a'; 'b'; 'c'; 'b'; 'd'; 'a'; 'b'|];;
let sl2 = [|'b'; 'd'; 'c'; 'a'; 'b'; 'a'|];;

