(* zadanie kominki, 14/15 *)
(* kominki k m tab *)
(* w kominku miesci sie k, chcemy zmaksymalizowac min (suma, m) + (max w kominku) *)

let max3 a b c = max a (max b c)

let kominki k m tab = 
  let maks = Array.make (k + 1) (-1) and n = Array.length tab in
    maks.(0) <- 0;
    for i = 0 to n - 1 do
      for j = k downto 0 do
        if tab.(i) + j <= k && maks.(j) <> -1 then
          maks.(j + tab.(i)) <- max3 maks.(j + tab.(i)) maks.(j) tab.(i)
      done
    done;
    let maximum = ref 0 in
    for j = 0 to k do
      maximum := max !maximum (min j m + maks.(j))
    done;
    !maximum;;

kominki 42 36 [|25; 15; 30|];;

kominki 42 36 [|25; 15; 34|];;
