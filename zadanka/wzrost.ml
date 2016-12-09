(* wzrost II kolos 11/12 *)
let lista = [3; 4; 0; -1; 2; 3; 7; 6; 7; 8];;

let wzrost l =
  if l = [] then []
  else let (w, t, d, m) = 
    List.fold_left (fun (wyn, tmp, dl, maks) el ->
                      if el >= List.hd tmp then
                        (wyn, (el :: tmp), dl + 1, maks)
                      else if dl > maks then
                        (tmp, [el], 1, dl)
                      else (wyn, [el], 1, maks))
                    ([List.hd l], [List.hd l], 1, 1) (List.tl l)
  in if d > m then List.rev t else List.rev w
    
