(* prepalindromy 13/14 *)
(* szukamy najdluzszego prefiksu bedacego palindromem *)
#load "kmp.cmo"

let rev tab = (* odwracamy tablice *)
  let n = Array.length tab in 
    Array.init n (fun i -> tab.(n - i - 1) )

let prepalindrom tab =
  let n = Array.length tab in
    let slowo = Array.append tab (rev tab) in (* doklejamy odwrocone slowo *)
      let p = Kmp.pref slowo in               (* w tablicy prefiksowej szukamy prefikso *)
        let i = ref p.(2 * n - 1) in          (* -sufiksu  krotszego niz slowo *)
          while !i >= n do
            i := p.(!i)
          done;
          !i + 1

let tab = [|'a'; 'a'; 'b'; 'a'; 'a'; 'b'; 'a'|];;

let tab2 = [|'a'; 'b'|];;

let tab3 = [|'a'; 'a'; 'a'; 'a'|];;

let tab4 = [|'c'; 'a'; 'c'; 'b'; 'c'; 'a'; 'c'; 'x'|];;
