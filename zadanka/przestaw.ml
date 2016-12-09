(* zadanie przestaw, II kolokwium 13/14 *)
let tablica = [|-7; -5; -5; -2; 0; 3; 3; 5; 11; 12|]

let przestaw tablica = 
  let n = Array.length tablica in
    let pom = Array.init n (fun a -> tablica.(a)) in
      let lewy = ref 0 and prawy = ref (n-1) and ind = ref (n - 1) in
        while !ind >= 0 do
          if abs pom.(!lewy) > abs pom.(!prawy) then
            begin
              tablica.(!ind) <- pom.(!lewy); 
              incr lewy
            end
          else
            begin
              tablica.(!ind) <- pom.(!prawy);
	      decr prawy
            end
          decr ind
        done 

