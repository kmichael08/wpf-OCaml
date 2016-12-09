let sqr a = a * a

let myszy k tab = 
  let n = Array.length tab in
    let prefiksy = Array.make n 0 and
        t = Array.make_matrix n n 0 and
        dynamik = Array.make_matrix (k + 1) n 0 in
          prefiksy.(0) <- tab.(0);
          for i = 1 to n - 1 do
            prefiksy.(i) <- prefiksy.(i-1) + tab.(i)
          done;
          let suma a b = if a = 0 then prefiksy.(b)
                         else prefiksy.(b) - prefiksy.(a - 1) in
            for i = 0 to n - 1 do
              for j = i to n - 1 do
                t.(i).(j) <- max 0 (suma i j - sqr (j - i))
              done
            done;
            for i = 0 to k do
              if i > 0 then dynamik.(i).(0) <- a.(0);
              for j = 1 to n - 1 do
                for l = 0 to j - 1 do
                  dynamik.(i).(j) <- if i = 0 then 0 
                            else max dynamik.(i).(j) (dynamik.(i - 1).(l) + t.(l + 1).(j))
                done
              done
            done;
            let maks = ref 0 in
              for i = 0 to n - 1 do
                if dynamik.(k).(i) > !maks then maks := dynamik.(k).(i)
              done;
              !maks

let a = [|1; 5; 1; 4; 3; 2; 7; 0|];;
myszy 2 a;;
