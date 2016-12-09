(* Zadanie cykle II kolos 11/12 *)

let tab = [|2; 1; 0; 5; 6; 4; 3; 8; 7|];

let cykl tab = 
  let n = Array.length tab in
    let vis = Array.make n false and j = ref 0
    and tmp = ref 1 and wyn = ref 1 in
      for i = 1 to (n - 1) do
        if (not vis.(i)) then
        vis.(i) <- true;
        tmp := 1;   
        j := tab.(i);
          while (not vis.(!j)) do
            incr tmp;
            vis.(!j) <- true;
            j := tab.(!j)
          done;
        wyn := max !wyn !tmp
      done;
    wyn
