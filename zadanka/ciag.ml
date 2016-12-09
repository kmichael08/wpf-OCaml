(* zadanie ciag, 13/14 *)

let primes n = (* zwraca liste liczb pierwszych mniejszych niz n posortowanych rosnaco *)
  let pierwsze = Array.make n true in
    pierwsze.(1) <- false;
    for i = 2 to n - 1 do
      if pierwsze.(i) then
        let j = ref (i * i) in
          while !j < n do
            pierwsze.(!j) <- false;
            j := !j + i
          done
    done;
    let rec lista acc i =
      if i = 1 then acc else
      if pierwsze.(i) then lista (i::acc) (i - 1)
      else lista acc (i - 1)
    in lista [] (n - 1)           

let ciag n = 
  let pierwsze = primes (n + 1) in
    let seq = Array.make (n + 1) 0 in
      let poprz = Array.make (n + 1) 0 in
        for i = 2 to n do 
          seq.(i) <- seq.(i - 1) + 1;
          poprz.(i) <- i - 1;
          List.iter (fun p -> if i mod p = 0 then
                       if seq.(i / p) + 1 < seq.(i) then
                         begin
                           seq.(i) <- seq.(i / p) + 1;
                           poprz.(i) <- i / p
                         end) pierwsze;
       done;
       let rec create i wyn =
         if i = 1 then (1::wyn) 
         else create poprz.(i) (i::wyn)
       in create n []
