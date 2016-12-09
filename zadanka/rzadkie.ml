let binary liczba = 
  let rec dwa wynik n =
    if n = 0 then wynik
    else if n mod 2 = 0 then dwa (0::wynik) (n/2)
    else dwa (1::wynik) (n/2)
  in dwa [] liczba

let liczba = 42;;
binary liczba;;

let rzadkie liczba =
  let rec fib suma lista a b jedynki =
    match lista with
    | []   -> if jedynki = 2 then suma - 1 else suma
    | h::t -> if h = 1 && jedynki = 2 then fib a t (a + b) a 1 
              else if h = 1 then fib (suma + a) t (a + b) a (jedynki + 1)
              else fib suma t (a + b) a 0
  in fib 0 (List.rev (binary liczba)) 1 1 0;;

rzadkie liczba;;
rzadkie 0;;
rzadkie 1;;
rzadkie 2;;
rzadkie 3;;
rzadkie 4;;
rzadkie 5;;
rzadkie 6;;
rzadkie 7;;
rzadkie 8;;
rzadkie 9;;
rzadkie 10;;
rzadkie 11;;
rzadkie 12;;
