(* zadanie rozne, II Kolos 12/13 *)
let a = [|-15; -25; -30; -33; -35; -35; -31; -25; -13; 4; 24|];;

let roznice a = (* tablica roznic *)
  let n = Array.length a in
    Array.init (n - 1) (fun i -> a.(i+1) - a.(i))

let sort_d a =
  let is = ref true and tab = roznice a in
    let n = Array.length tab in
      for i = 1 to (n - 1) do
        is := !is && tab.(i) > tab.(i-1)
      done;
      !is

let rozne a = 
  let n = Array.length a in
    if n <= 1 then true
    else let lewy = ref 1 and prawy = ref (n - 1) and sr = ref (n / 2) and dif = ref true in
      while lewy < prawy do
        if a.(!sr) - a.(!sr - 1) < 0 then lewy := !sr + 1
        else if a.(!sr) - a.(!sr - 1) > 0 then prawy := !sr - 1
        else
        begin 
          dif := false;
          lewy := !sr; 
          prawy := !sr
        end;
        sr := (!lewy + !prawy) / 2
      done;
      a.(!lewy) <> a.(!lewy - 1) && !dif
   
