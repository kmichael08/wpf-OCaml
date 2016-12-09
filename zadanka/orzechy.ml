(* zadanie orzechy *)

let t = [|0; 0; 4; 1; 2; 0|];;
let t2 = [|3; 3; 8; 11; 3; 2|];;
let t3 = [|1; 3; 9; 8; 1; 2|];;
let t4 = [|2; 1; 1; 2; 0; 12|];;

let tab = [|t; t2; t3; t4|];;

let orzechy k tab =
  let n = Array.length tab and m = Array.length tab.(0) in
    let suma = Array.make_matrix (n + 1) (m + 1) 0 in (* "sumy prefiksowe " *)
      for i = 1 to n do 
        for j = 1 to m do
          suma.(i).(j) <- 
            suma.(i - 1).(j) + suma.(i).(j - 1) + tab.(i - 1).(j - 1) - suma.(i - 1).(j - 1)
        done
      done;
      let total (x1, y1) (x2, y2) = (* oblicza dokladnie sume w prostokacie *)
        if y1 >=m || y2 >= m then failwith "o co kaman" else
        suma.(x2 + 1).(y2 + 1) - suma.(x2 + 1).(y1) - suma.(x1).(y2 + 1) + suma.(x1).(y1)
      in let maks = ref 0 in
      let check szer dl = (* sprawdza wszystkie prostokaty o danej dlugosci i szerokosci *)
        for i = 0 to n - szer do 
          for j = 0 to m - dl do
            if total (i, j) (i + szer - 1, j + dl - 1) > !maks then 
              maks := total (i, j) (i + szer - 1, j + dl - 1)
          done
        done;
        in let i = ref 1 in 
          while !i * !i <= k do (* wszystkie dzielniki mozliwe zapodaje *)
            if k mod !i = 0 then
              begin
                check !i (k / !i);
                check (k / !i) !i
              end;
              incr i 
         done;
         !maks
            
