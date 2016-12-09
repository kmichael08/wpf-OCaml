let tab = [|5; 3; 8; 7; 4|]

let ceil x w = if x mod w = 0 then x / w else (x / w) + 1 (* sufit z x /. w *)

let wart x c m =
  let w = m / c in ceil x w

let most tab k =
  let n = Array.length tab in
    let c = ref tab.(0) in
      for i = 1 to n - 1 do
        if tab.(i) < !c then c := tab.(i)
      done;
      let mini = Array.make_matrix n n (-1) in (* minimum na kazdym przedziale *)
        for i = 0 to n - 1 do
          mini.(i).(i) <- tab.(i);
          for j = i + 1 to n - 1 do
            mini.(i).(j) <- min mini.(i).(j - 1) tab.(j)
          done
        done;
        let t = Array.make_matrix n n 0 in
          for i = 0 to n - 1 do
            for j = i to n - 1 do
              t.(i).(j) <- wart (j - i + 1) !c mini.(i).(j)
            done
          done;
          let dynamik = Array.make_matrix (k + 1) n max_int in
            for i = 0 to k do
              for j = i to n - 1 do
                if i = 0 then dynamik.(i).(j) <- t.(i).(j)
                else
                for l = 0 to j - 1 do
                  dynamik.(i).(j) <- min dynamik.(i).(j) (max dynamik.(i - 1).(l) t.(l + 1).(j))
                done
              done
            done;
            dynamik.(k).(n - 1)
