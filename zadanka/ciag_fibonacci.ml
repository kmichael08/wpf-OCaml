(* Ciag kolos 13/14 poprawkowy *)
(* x1 = 1, x(i+1) = xi + fi dla pewnej liczby fibonacciego *)
(* najkrotszy ciag taki konczacy sie w n znalezc *)

let fibo n = (* lista liczb fibonacciego mniejszych od n, malejaco *)
  let rec tworz a b acc =
    if a >= n then acc
    else tworz (a + b) a (a::acc)
  in tworz 1 1 []

let ciag n = 
  let fibonacci = fibo n and
  dlugosc = Array.make (n + 1) n and
  poprz = Array.make (n + 1) 0 in
    dlugosc.(1) <- 1;
    for i = 2 to n do
      List.iter (fun fib -> if i > fib && dlugosc.(i - fib) + 1 < dlugosc.(i) then
                              begin
                                dlugosc.(i) <- dlugosc.(i - fib) + 1;
                                poprz.(i) <- i - fib
                              end) fibonacci
    done;
    let rec create acc j =
      if j = 0 then acc
      else create (j::acc) poprz.(j)
    in create [] n
