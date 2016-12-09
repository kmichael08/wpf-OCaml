(* najdluzszy cykl w tablicy *)
let tablica = [|2; 1; 0; 5; 6; 4; 3; 8; 7|];;

let cykl tab = 
  let n = Array.length tab in
  let log = Array.make n false  (* false - nieodwiedzony, true odwiedzony *)
  and mx = ref 0 and j = ref 0 and tmp = ref 0
  in
  for i = 0 to n-1 do
    if not log.(i) then
      begin
        log.(i) <- true;
        tmp := 1;
        j := tab.(i);
        
        while !j <> i do
          log.(!j) <- true;
          incr tmp;
          j := tab.(!j);
        done;
        mx := max !mx !tmp;
      end
     done;
     mx

(* spoko idea *)
let f = 
  let cnt = ref 0
  in
  function () ->
    incr cnt;
    !cnt
