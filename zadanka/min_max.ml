(* zadanie min_max *)
let tab = [|13; 17; 20; 28; 3; 5; 8; 10|];;

let min_max tab = 
  match tab with
  | [||] -> failwith "Pusta"
  | _  ->
    let n = Array.length tab in
    let lewy = ref 0 and prawy = ref (n - 1) in
      if tab.(0) < tab.(n - 1) then (tab.(0), tab.(n - 1))
      else 
        begin
        while !lewy + 1 < !prawy do
          let sr = (!lewy + !prawy) / 2 in  (* lewy <= sr < prawy *)
            if tab.(!lewy) < tab.(sr) then lewy := sr (* tab.(!lewy) > tab.(!prawy) *)
            else prawy := sr
        done;
        (tab.(!prawy), tab.(!lewy))
        end

(* zrobimy rotowanie tablicy *)
let rotuj tab k =
  let n = Array.length tab in
    let old = Array.init n (fun el -> tab.(el)) in
      for i = 0 to (n - 1) do
        let nast = (i + k) mod n in
          tab.(nast) <- old.(i)
      done
    
