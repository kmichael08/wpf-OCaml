(* rozklad liczby parzystej na sume dwoch liczb pierwszych *)

let sito n =
  let tab = Array.init n (fun x -> if x <= 1 then true else false) in
    for i = 2 to n - 1 do
      if tab.(i) = false then
        let j = ref (2 * i) in
          while !j < n do
            tab.(!j) <- true;
            j := !j + i
          done
    done;
    tab

let prime_list n = List.rev (snd ( 
  Array.fold_left (fun (nr,lista) el -> if el then (nr + 1,lista) else (nr + 1,(nr::lista))) (0,[]) (sito n)))


let goldbach n =
  let primes = prime_list n and is_prime = sito n in
    let i = ref 4 in
    while !i < n do
      let prawda = ref false in
        let rec przejdz l =
            match l with
	    | h::t -> if h < !i then begin if is_prime.(!i - h) then prawda := true else przejdz t end
	    | [] -> ()
        in przejdz primes;
      if !prawda then Printf.printf "%d \n" !i else Printf.printf "Zle - %d \n" !i;
      i := !i + 2
    done;;


