 let silnie n =
  let rec stos acc i =
    let top = snd (List.hd acc) in
      if i * top > n then acc
      else stos ((i, (i * top))::acc) (i + 1)
    in let s = stos [(1, 1)] 2 in
      let rec przejscie wynik lista liczba =
        if liczba = 0 then wynik
        else let (ind, wart) = List.hd lista in
          if wart > liczba then przejscie wynik (List.tl lista) liczba 
          else przejscie (ind::wynik) lista (liczba - wart)
      in przejscie [] s n

(* dodac rozwiazanie dynamiczne - to byl zachlan *)

let silnie2 n =
  let tab = Array.make (n + 1) [] in
    let factorial = Array.make (n + 1) (-1) in (* czy ta liczba jest silnia *)
      let rec fact i s =
        if s <= n then 
          begin
            factorial.(s) <- i;
            fact (i + 1) ((i + 1) * s)
          end
      in fact 1 1;
      tab.(1) <- [1];
        for i = 2 to n do
          let min = ref i in
            if factorial.(i) <> (-1) then tab.(i) <- [factorial.(i)]
            else 
              for j = 1 to (i + 1)/2 do
                let sum = List.length tab.(j) + List.length tab.(i - j) in
                  if sum < !min then 
                    begin
                      min := sum;
                      tab.(i) <- tab.(j) @ tab.(i - j)
                    end;
              done
        done;
      tab.(n)

let check n =
  List.length (silnie2 n) = List.length (silnie n)

for i = 1 to 1000000 do
  if not (check i) then Printf.printf "%d \n" i
done
