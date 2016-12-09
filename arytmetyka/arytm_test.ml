(* program do testowania, z wbudowantymi testami *)

let ile_zle=ref 0  (* liczba pomylek *)

(* procedura sprawdzajaca warunek w tescie numer *)
let check numer warunek=
  if not warunek then
  begin
    Printf.printf "Blad na tescie nr %d !!!\n" numer;
    incr ile_zle
  end
  else
    Printf.printf "OK  nr %d. \n" numer;;

(*wartosc bezwzgledna *)
let abs x=
  if x>=0. then x else (-.x);;

(* porownanie dwoch floatow *)
let equal x y=
  if x = infinity && y = infinity then true
  else if x = neg_infinity && y = neg_infinity then true
  else let epsilon = 0.00001
       in  (abs (x -. y) < epsilon);;

open Arytmetyka;;  


Printf.printf "***** Konstruktory i selektory ********\n";;

let a=wartosc_od_do 1. 2.;;
let b=wartosc_od_do 3. 4.;;

check 1 (in_wartosc a 1.5);;
check 2 (not (in_wartosc a 2.1));;
check 3 (in_wartosc b 4.);;

let c=wartosc_dokladna (-7.);;

check 4 (in_wartosc c (-7.));;
check 5 (not (in_wartosc c (-6.)));;

let d=wartosc_dokladnosc (-2.) 10.;;

check 6 (min_wartosc d = (-2.2));;
check 7 (max_wartosc d = (-1.8));;
check 8 (not (in_wartosc d (-1.7)));;
check 9 (min_wartosc c = (-7.));;

Printf.printf "*********** Dodawanie i odejmowanie **********\n";;
(* testy na neg_infinity, infinity, nan, 
wszystkie opcje dodawania i odejmowania 
pomocniczy konstruktor antyinterwalu
*)

let a=wartosc_od_do 2. 5.;;
let b=wartosc_od_do 3. 7.;;
let c=wartosc_od_do (-2.) (-1.);;
let d=wartosc_od_do (-7.) (-3.);;
let e=wartosc_od_do (-4.) 8.;;
let f=wartosc_od_do (-10.) 2.;;

check 10 (min_wartosc (plus a b) = 5.);;
check 11 (min_wartosc (plus a c) = 0.);;
check 12 (max_wartosc (plus b c) = 6.);;
check 13 (max_wartosc (plus b d) = 4.);;
check 14 (sr_wartosc  (plus b f) = 1.);;
check 15 (sr_wartosc  (plus c e) = 0.5);;
check 16 (max_wartosc (minus b c) = 9.);;


Printf.printf "********** Dzielenie i mnozenie *************\n";;

check 50 (equal (min_wartosc (podzielic a b)) (2. /. 7.));;
check 51 (equal (max_wartosc (podzielic c f)) (infinity));;
check 52 (in_wartosc (podzielic c f) 0.1);;
check 53 (not (in_wartosc (podzielic c f) 0.05));;
check 54 (in_wartosc (podzielic c f) 1.);;
check 55 (in_wartosc (podzielic c f) (-1.));;
check 56 (in_wartosc (podzielic c f) (-2.));;
check 57 (not (in_wartosc (podzielic c f) (-0.4)));;

let a=wartosc_od_do (-5.) (-2.);;
let b=wartosc_od_do 2. 5.;;
let c=wartosc_od_do (-3.) 3.;;
let d=wartosc_od_do (-3.) 0.;;
let e=wartosc_od_do 0. 3.;;

check 55 (in_wartosc (podzielic a b) (-1.));;
check 56 (in_wartosc (podzielic a c) (-1.));;
check 57 (not (in_wartosc (podzielic a d) (-1.)));;
check 58 (in_wartosc (podzielic a e) (-1.));;
check 59 (not (in_wartosc (podzielic a a) (-1.)));;
check 60 (not (in_wartosc (podzielic b b) (-1.)));;
check 61 (in_wartosc (podzielic b c) (-1.));;
check 62 (in_wartosc (podzielic b d) (-1.));;
check 63 (not (in_wartosc (podzielic b e) (-1.)));;
check 64 (in_wartosc (podzielic c c) (-1.));;
check 65 (in_wartosc (podzielic c d) (-1.));;
check 66 (in_wartosc (podzielic c e) (-1.));;
check 67 (not (in_wartosc (podzielic d d) (-1.)));;
check 68 (in_wartosc (podzielic d e) (-1.));;
check 69 (not (in_wartosc (podzielic e e) (-1.)));;
check 155 (not (in_wartosc (podzielic a b) (-100.)));;
check 156 (in_wartosc (podzielic a c) (-100.));;
check 157 (not (in_wartosc (podzielic a d) (-100.)));;
check 158 (in_wartosc (podzielic a e) (-100.));;
check 159 (not (in_wartosc (podzielic a a) (-100.)));;
check 160 (not (in_wartosc (podzielic b b) (-100.)));;
check 161 (in_wartosc (podzielic b c) (-100.));;
check 162 (in_wartosc (podzielic b d) (-100.));;
check 163 (not (in_wartosc (podzielic b e) (-100.)));;
check 164 (in_wartosc (podzielic c c) (-100.));;
check 165 (in_wartosc (podzielic c d) (-100.));;
check 166 (in_wartosc (podzielic c e) (-100.));;
check 167 (not (in_wartosc (podzielic d d) (-100.)));;
check 168 (in_wartosc (podzielic d e) (-100.));;
check 169 (not (in_wartosc (podzielic e e) (-100.)));;
check 255 (not (in_wartosc (podzielic a b) (-0.1)));;
check 256 (not (in_wartosc (podzielic a c) (-0.1)));;
check 257 (not (in_wartosc (podzielic a d) (-0.1)));;
check 258 (not (in_wartosc (podzielic a e) (-0.1)));;
check 259 (not (in_wartosc (podzielic a a) (-0.1)));;
check 260 (not (in_wartosc (podzielic b b) (-0.1)));;
check 261 (not (in_wartosc (podzielic b c) (-0.1)));;
check 262 (not (in_wartosc (podzielic b d) (-0.1)));;
check 263 (not (in_wartosc (podzielic b e) (-0.1)));;
check 264 (in_wartosc (podzielic c c) (-0.1));;
check 265 (in_wartosc (podzielic c d) (-0.1));;
check 266 (in_wartosc (podzielic c e) (-0.1));;
check 267 (not (in_wartosc (podzielic d d) (-0.1)));;
check 268 (in_wartosc (podzielic d e) (-0.1));;
check 269 (not (in_wartosc (podzielic e e) (-0.1)));;
check 355 (not (in_wartosc (podzielic a b) (0.1)));;
check 356 (not (in_wartosc (podzielic a c) (0.1)));;
check 357 (not (in_wartosc (podzielic a d) (0.1)));;
check 358 (not (in_wartosc (podzielic a e) (0.1)));;
check 359 (not (in_wartosc (podzielic a a) (0.1)));;
check 360 (not (in_wartosc (podzielic b b) (0.1)));;
check 361 (not (in_wartosc (podzielic b c) (0.1)));;
check 362 (not (in_wartosc (podzielic b d) (0.1)));;
check 363 (not (in_wartosc (podzielic b e) (0.1)));;
check 364 (in_wartosc (podzielic c c) (0.1));;
check 365 (in_wartosc (podzielic c d) (0.1));;
check 366 (in_wartosc (podzielic c e) (0.1));;
check 367 (in_wartosc (podzielic d d) (0.1));;
check 368 (not (in_wartosc (podzielic d e) (0.1)));;
check 369 (in_wartosc (podzielic e e) (0.1));;
check 455 (not (in_wartosc (podzielic a b) (0.)));;
check 456 (not (in_wartosc (podzielic a c) (0.)));;
check 457 (not (in_wartosc (podzielic a d) (0.)));;
check 458 (not (in_wartosc (podzielic a e) (0.)));;
check 459 (not (in_wartosc (podzielic a a) (0.)));;
check 460 (not (in_wartosc (podzielic b b) (0.)));;
check 461 (not (in_wartosc (podzielic b c) (0.)));;
check 462 (not (in_wartosc (podzielic b d) (0.)));;
check 463 (not (in_wartosc (podzielic b e) (0.)));;
check 464 (in_wartosc (podzielic c c) (0.));;
check 465 (in_wartosc (podzielic c d) (0.));;
check 466 (in_wartosc (podzielic c e) (0.));;
check 467 (in_wartosc (podzielic d d) (0.));;
check 468 (in_wartosc (podzielic d e) (0.));;
check 469 (in_wartosc (podzielic e e) (0.));;
check 555 (not (in_wartosc (podzielic a b) (1.)));;
check 556 (in_wartosc (podzielic a c) (1.));;
check 557 (in_wartosc (podzielic a d) (1.));;
check 558 (not (in_wartosc (podzielic a e) (1.)));;
check 559 (in_wartosc (podzielic a a) (1.));;
check 560 (in_wartosc (podzielic b b) (1.));;
check 561 (in_wartosc (podzielic b c) (1.));;
check 562 (not (in_wartosc (podzielic b d) (1.)));;
check 563 (in_wartosc (podzielic b e) (1.));;
check 564 (in_wartosc (podzielic c c) (1.));;
check 565 (in_wartosc (podzielic c d) (1.));;
check 566 (in_wartosc (podzielic c e) (1.));;
check 567 (in_wartosc (podzielic d d) (1.));;
check 568 (not (in_wartosc (podzielic d e) (1.)));;
check 569 (in_wartosc (podzielic e e) (1.));;
check 655 (not (in_wartosc (podzielic a b) (100.)));;
check 656 (in_wartosc (podzielic a c) (100.));;
check 657 (in_wartosc (podzielic a d) (100.));;
check 658 (not (in_wartosc (podzielic a e) (100.)));;
check 659 (not (in_wartosc (podzielic a a) (100.)));;
check 660 (not (in_wartosc (podzielic b b) (100.)));;
check 661 (in_wartosc (podzielic b c) (100.));;
check 662 (not (in_wartosc (podzielic b d) (100.)));;
check 663 (in_wartosc (podzielic b e) (100.));;
check 664 (in_wartosc (podzielic c c) (100.));;
check 665 (in_wartosc (podzielic c d) (100.));;
check 666 (in_wartosc (podzielic c e) (100.));;
check 667 (in_wartosc (podzielic d d) (100.));;
check 668 (not (in_wartosc (podzielic d e) (100.)));;
check 669 (in_wartosc (podzielic e e) (100.));;





Printf.printf "%d testow zle\n" !ile_zle;;
