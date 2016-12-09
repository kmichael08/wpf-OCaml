open List;;

let fit c l =
  let dlugosc = length l 
  in 
  let rec przejscie l1 l2 mini ind1 ind2 = 
    if ind1 + ind2 > dlugosc + 1 then mini
    else match l1, l2 with
         | h::t, g::o ->
           if h + g + c > 0 then 
            przejscie l1 o (min mini (abs (h + g + c))) ind1 (ind2 + 1)
           else przejscie t l2 (min mini (abs (h + g + c))) (ind1 + 1) ind2
         | _, _ -> mini
  in przejscie l (rev l) (abs (2 * (hd l) + c )) 1 1

let l = [-28; -25; -15; -1; 4; 8; 15; 60];;

let c = 42;;

