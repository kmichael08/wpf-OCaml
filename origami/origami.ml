(* ========================================================================= *)
(*                         Zadanie Origami                                   *)
(*         Autor : Michał Kuźba, Reviewer : Michał Pawłowski                 *)
(* ========================================================================= *)
type point = float * float

type kartka = point -> int

(* ========================================================================= *)
(*                        funkcje pomocnicze                                 *)
(* ========================================================================= *)

let skalarny p r q = (* iloczyn skalarny wektora r-p i q-p *)
  let x1 = fst r -. fst p
  and y1 = snd r -. snd p
  and x2 = fst q -. fst p
  and y2 = snd q -. snd p
  in 
  x1 *. x2 +. y1 *. y2

let rzut p r q = (* rzut punktu q  na prosta pr *)
  let f = skalarny p r q /. skalarny p r r
  in (fst p +. f *. (fst r -. fst p), snd p +. f *. (snd r -. snd p))
  
let symetria p r q = 
  let x1, y1 = q and x2, y2 = rzut p r q
  in (2. *. x2 -. x1, 2. *. y2 -. y1)
(* odbicie symetryczne punktu q wzgledem prostej pr - odbicie q wzgl. rzutu *)

let sqr x = x *. x

let abs x = if x < 0. then -.x else x

let epsilon = 0.00000000001 (* dokladnosc *)

let compare a b = abs (a -. b) < epsilon  (* porownanie na floatach *)

let strona (p1, p2) (q1, q2) (r1, r2) = (* po ktorej stronie pr lezy punkt q *)
  let wektorowy = ((r1 -. p1) *. (q2 -. p2)) -. ((r2 -. p2) *. (q1 -. p1))
  in 
    if compare wektorowy 0. then 0
    else if wektorowy < 0. then -1
    else 1
    
(* -1 po lewej stronie , 1 po prawej, 0 na prostej  *)

(* ===================================================================== *)

let prostokat (x1, y1) (x2, y2) = (* kartka prostokatna *)
  (fun (px, py) -> if px > x2 || px < x1 || py > y2 || py < y1 then 0 else 1)

let kolko (x1, y1) r =   (* kartka okragla *)
  (fun (px, py) -> if sqr (px -. x1) +. sqr (py -. y1) > sqr r  then 0 else 1)

let zloz p r k =
  (fun q -> if strona p r q = 0 then k q
            else if strona p r q = 1 then 0
            else let q2 = symetria p r q in k q + k q2)

let skladaj l k = (* zwracamy funkcje kartka po zlozeniach *)
  List.fold_left (fun wynik (p, r) -> zloz p r wynik ) k l
  
(* ======================================================================== *)
(*                                  TESTY                                   *)
(* ======================================================================== *)
(* Proste testy na kartki *)

let k = prostokat (0., 0.) (11., 4.);; 

assert(k (0., 0.) = 1);;
assert (k (12., 3.) = 0);;
assert (k (11., 2.) = 1);;
assert (k (-5., 3.) = 0);;
assert (k (-4., -2.) = 0);;
assert (k (5., -2.) = 0);;
assert (k (3., 3.999999) = 1);;

let circle = kolko (1., 1.) 3.;;

assert (circle (1., 1.) = 1);;
assert (circle (2., 2.) = 1);;
assert (circle (0., 3.) = 1);;
assert (circle (1., 4.) = 1);;
assert (circle (1., 4.000000001) = 0);;
assert (circle (3., 3.2360) = 1);;
assert (circle (3., 3.2361) = 0);;


(* Testy na funkcje strona *)
let p = (1., 2.) and r = (5., 4.);;

assert (strona p r (100., 0.) = 1);;
assert (strona p r (5., 2.) = 1);;
assert (strona p r (3., 3.) = 0);;
assert (strona p r (0., 1.5) = 0);;
assert (strona p r (0., 1.5001) = -1);;
assert (strona p r (-10., 0.) = -1);;

(* Testy na funkcje zloz *)
let p, r = ((3., 0.) , (3., 4.));;

assert (zloz p r k (0., 0.) = 2);;
assert (zloz p r k (1., 1.) = 2);;
assert (zloz p r k (-2., 3.) = 1);;
assert (zloz p r k (-5., 2.) = 1);;
assert (zloz p r k (-5.0001, 2.) = 0);;
assert (zloz p r k (-3., -2.) = 0);;
assert (zloz p r k (3., 2.) = 1);;
assert (zloz p r k (3., 0.) = 1);;
assert (zloz p r k (3., 5.) = 0);;
assert (zloz p r k (3., -0.00001) = 0);;
assert (zloz p r k (3.11, 3.) = 0);;
assert (zloz p r k (11., 3.) = 0);;
assert (zloz p r k (12., 3.) = 0);;

(* odwracamy zwrot pr *)
assert (zloz r p k (0., 0.) = 0);;
assert (zloz r p k (1., 1.) = 0);;
assert (zloz r p k (-2., 3.) = 0);;
assert (zloz r p k (3., 2.) = 1);;
assert (zloz r p k (3., 5.) = 0);;
assert (zloz r p k (3.11, 3.) = 2);;
assert (zloz r p k (6., 1.) = 2);;
assert (zloz r p k (6.11, 2.) = 1);;
assert (zloz r p k (5., 3.999999) = 2);;

let p, r = ((5., 0.), (9., 4.));;

assert (zloz p r k (7., 2.) = 1);;
assert (zloz p r k (4., -1.) = 0);; 
assert (zloz p r k (10., 2.) = 0);;
assert (zloz p r k (5.0001, 0.) = 0);;
assert (zloz p r k (2., 2.) = 1);;
assert (zloz p r k (4.9, 0.000001) = 1);;
assert (zloz p r k (6., 3.) = 2);;
assert (zloz p r k (7., 3.) = 2);;
assert (zloz p r k (5.5, 5.5) = 1);;

(* kartka okragla *)
let kolo = kolko (1., 1.) 3.;;

let p, r = ((2.,0.), (2.,500.));;

assert (zloz p r kolo (2., 1.99) = 1);; 
assert (zloz p r kolo (2., -100.) = 0);;
assert (zloz p r kolo (2.1, 0.) = 0);;
assert (zloz p r kolo (1., 2.) = 2);;
assert (zloz p r kolo (0., 1.) = 2);;
assert (zloz p r kolo (0., 1.01) = 1);;
assert (zloz p r kolo (1., 1.5) = 2);;
assert (zloz p r kolo (1., 4.) = 1);;

(* Testy na funkcje skladaj *)
let pr1 = ((9., 1.), (9., 3.5));;
let pr2 = ((3., 1.), (5., 1.));;

let lista = [pr1; pr2];;

let f = skladaj lista k;;

assert (f (9., 1.) = 1);;
assert (f (2., 3.) = 1);; 
assert (f (2., 1.5) = 2);;
assert (f (2., 0.3) = 0);;
assert (f (8., 3.) = 2);;
assert (f (8., 2.) = 4);;
assert (f (8., 1.4) = 4);;
assert (f (8., 0.5) = 0);;
assert (f (10., 3.) = 0);;
assert (f (10., 0.2) = 0);;
assert (f (3., 7.) = 0);;
assert (f (8., 1.) = 2);; 

