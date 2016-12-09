(* zadanie banknoty kolos III 15/16 *)
(* mamy kwote i liste banknotow, znalezc najmniejsza liczbe nominalow potrzebna do wyplaty *)

let banknoty k b =
  let liczba = Array.make (k + 1) (k + 1) 
  and last = Array.make (k + 1) 0 in
    liczba.(0) <- 0;
    List.iter (fun kwota -> 
      for i = 0 to k - kwota do 
        if last.(i) = kwota then
          begin
            if liczba.(i) < liczba.(i + kwota) then
              begin
                liczba.(i + kwota) <- liczba.(i);
                last.(i + kwota) <- kwota
              end
          end
        else
          begin
            if liczba.(i) + 1 < liczba.(i + kwota) then
              begin
                liczba.(i + kwota) <- liczba.(i) + 1;
                last.(i + kwota) <- kwota
              end
          end
      done) b;
    if liczba.(k) = k + 1 then (-1) else liczba.(k)


let b = [10; 15; 11; 12; 20];;

banknoty 49 b;;
