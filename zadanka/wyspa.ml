(* zadanie wyspa z kolokwium III 15/16 *)
(* chcemy znalezc najbardziej wewnetrzna wyspe *)
let wyspa map =
  let n = Array.length map and m = Array.length map.(0) in
    let mapa = Array.make_matrix (n + 2) (m + 2) false in
      for i = 1 to n do
        for j = 1 to m do
          mapa.(i).(j) <- map.(i - 1).(j - 1) 
        done
      done;
      let kol = ref (Queue.create())
      and ruchy_lad x y = [(x + 1, y); (x - 1, y); (x, y + 1); (x, y - 1)] in
        let ruchy_woda x y = 
          (ruchy_lad x y) @ [(x + 1, y + 1); (x + 1, y - 1); (x - 1, y + 1); (x - 1, y - 1)] 
        and maks = ref 0 and ranga = Array.make_matrix (n + 2) (m + 2) (-1) in
          let is_good a b = a >= 0 && b >= 0 && a < n + 2 && b < m + 2 && ranga.(a).(b) = -1 in
            let bfs waga (x, y) =
              let ruchy = if mapa.(x).(y) then ruchy_lad else ruchy_woda in
                Queue.add (x, y) !kol;
                while not (Queue.is_empty !kol) do
                  let (a, b) = Queue.pop !kol in
                    ranga.(a).(b) <- waga;
                    List.iter (fun (c, d) -> if is_good c d && mapa.(c).(d) = mapa.(a).(b) then
                                 Queue.add (c, d) !kol) (ruchy a b)
                done
            in bfs 0 (0, 0);
            for i = 1 to n + 1 do
              for j = 1 to m + 1 do
                if ranga.(i).(j) = -1 then
                  begin
		    let waga = if mapa.(i).(j) then ranga.(i - 1).(j) + 1
			       else ranga.(i - 1).(j) in
		      bfs waga (i, j);
		      maks := max !maks waga
		  end
	      done
            done;
	    !maks

let m1 = [|true; false; false; false; true; false; false|];;
let m2 = [|true; false; false; true; true; false; false|];;
let m3 = [|true; true; true; true; true; false; false|];;
let m4 = [|true; true; false; true; true; true; true|];;
let m5 = [|true; false; true; false; false; false; true|];;
let m6 = [|true; false; true; true; true; false; true|];;
let m7 = [|true; true; false; false; false; false; true|];;
let m8 = [|false; true; true; true; true; true; true|];;
let m9 = [|false; false; true; true; true; true; false|];;
let m10 = [|false; false; false; false; false; true; false|];;
let m11 = [|false; true; true; true; false; false; false|];;
let m12 = Array.make 7 false;;

let map = [|m1; m2; m3; m4; m5; m6; m7; m8; m9; m10; m11; m12|];;
