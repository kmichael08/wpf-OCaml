open List;;

let lista = [2; 7; 3; 1; 4; 2; 8; 1; 3];;

let l_max lista = rev (fold_left (fun result el -> if el > hd result then (el::result) 
                                              else ( (hd result)::result) )
                            [hd lista] (tl lista) )

let r_max lista = snd( fold_right (fun el (maks, result) -> if el > maks then (el, (el::result))
                                               else (maks, maks::result) )
                             lista (0,[]))


let lewa maksy lista = map2 (fun el1 el2 -> el1 - el2) maksy lista

let dolek lista = fold_left2 (fun hole el1 el2 -> if (min el1 el2) > hole then (min el1 el2)
                                                  else hole )
                  0 (lewa (l_max lista) lista) (lewa (r_max lista) lista)
