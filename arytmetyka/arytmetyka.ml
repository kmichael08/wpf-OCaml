(*                Michał Kuźba - zadanie arytmetyka                     *)

(* ==================================================================== *)
(*                     Definicja typu                                   *)

(* Interval (x, y) - przedzial [x, y],
   Antiinterval (x, y) - dopelnienie przedzialu (x, y) do R  *)
type wartosc = Interval of float * float
             | Antiinterval of float * float;;

(* ==================================================================== *)
(*                       Konstruktory                                   *)
(* ==================================================================== *)

(* czy stala rowna nan *)
let is_nan a =
  if a <> a then true else false;;

(* para elementow uporzadkowana rosnaco *)
let sortpara x y =
  if x < y then Interval (x, y) else Interval (y, x)

(*  [x-p%,x+p%], warunek p>0  *)
let wartosc_dokladnosc x p =
  let r = (p /. 100.) *. x
    in sortpara (x -. r) (x +. r);;

(* [x,y], warunek x<=y     *)
let wartosc_od_do x y =
  Interval (x, y);;

(* [x;x] *)
let wartosc_dokladna x =
  Interval (x, x);;

(* ==================================================================== *)
(*                         Selektory                                    *)
(* ==================================================================== *)

(* czy x zawarte w w  *)
let in_wartosc w x =
  match w with
  | Interval (lewy, prawy)     -> if (x >= lewy && x <= prawy) then true 
                                   else false
  | Antiinterval (lewy, prawy) -> if (x <= lewy || x >= prawy) then true 
                                   else false;;
                                   
(* najmniejsza mozliwa wartosc wartosc *)
let min_wartosc w =
  match w with
  | Interval (x, _)     -> x
  | Antiinterval (_, _) -> neg_infinity;;

(* najwieksza mozliwa wartosc *)
let max_wartosc w =
  match w with
  | Interval (_, y)     -> y
  | Antiinterval (_, _) -> infinity;;

(* srednia wartosc, jesli nieokreslona to nan *)
let sr_wartosc w =
  match w with 
  | Interval (x, y)     -> (x +. y) /. 2.
  | Antiinterval (_, _) -> nan;;          

(* ==================================================================== *)
(*                      Modyfikatory                                    *)
(* ==================================================================== *)

(* przedzial przeciwny do przedzialu [a,b], przedzial -w *)
let opp w =
  match w with
  | Interval (a, b)     -> Interval (-.b, -.a)
  | Antiinterval (a, b) -> Antiinterval (-.b, -.a);;

(*dodawanie wartosci *)
let rec plus w v =
  match w,v with
  | Interval (a, b), Interval (c, d)         -> Interval (a +. c, b +. d)
  | Interval (a, b), Antiinterval (c, d)     -> 
      if (is_nan a || is_nan b) then Interval (nan, nan)
      else let x, y = (max (c +. a) (c +. b)), (min (d +. b) (d+.a)) 
           in if x < y then Antiinterval (x, y)
           else Interval (neg_infinity, infinity)
      
  | Antiinterval (a, b), Interval (c, d)     -> plus v w 
                                               (* dodawanie przemienne *)
  | Antiinterval (a, b), Antiinterval (c, d) -> 
      Interval (neg_infinity, infinity);;  
                                               
      
(* w - v = w + (-v) *)
let minus w v =
  plus w (opp v);;

(* funkcja minimum z uwzglednieniem nan *)
let mini a b =
  if is_nan a = true then b
  else if is_nan b = true then a
  else if a < b then a
  else b;;

(* funkcja maximum z uwzglednieniem nan *)  
let maxi a b =
  if is_nan a = true then b
  else if is_nan b = true then a
  else if a > b then a
  else b;;
  

(* Interval (minimum,maksimum z 4 liczb) *)
let minmax4 a b c d = 
  Interval (mini (mini a b) (mini c d), maxi (maxi a b) (maxi c d));; 

(* mnozenie wartosci *)
let rec razy w v=
  match w, v with
  | Interval (0., 0.), Interval (c, d)    -> 
      if c < infinity && d > neg_infinity then Interval (0., 0.)
      else Interval (nan , nan)
                                                  
  | Interval (c, d), Interval (0., 0.)        -> razy v w
  | Interval (a, b), Interval (c, d)          -> 
      let m1, m2, m3, m4 = (a *. c), (a *. d), (b *. c), (b *. d)
      in minmax4 m1 m2 m3 m4
  | Interval (0., 0.), Antiinterval (_, _)  -> Interval (0., 0.)   
  | Interval (a, b), Antiinterval (c, d)  -> 
      if (is_nan a || is_nan b) then Interval (nan, nan)
         else let x, y = if c >= 0. then 
                     if a >= 0. then (b *. c), (a *. d)
                     else if b <= 0. then (b *. d), (a *. c) (* bylo b * d *)
                     else infinity, neg_infinity
                                   
                 else if (c < 0. && d >= 0.) then 
                     if a >= 0. then (a *. c) , (a *. d)
                     else if b <= 0. then (b *. d), (b *. c) 
                     else infinity, neg_infinity
                     
                 else if a >= 0. then (a *. c), (b *. d)
                      else if b <= 0. then (a *. d), (b *. c)
                      else infinity, neg_infinity
                                                                          
      in
      let x2, y2 = 
        if (is_nan x = false && is_nan y = false) then x, y
        else if is_nan x = true then 0., y
        else x, 0.
      in 
      if x2 < y2 then Antiinterval (x2, y2)
      else Interval (neg_infinity, infinity)
      
  | Antiinterval (a , b), Interval (c , d)     -> razy v w
                                             (* przemiennosc mnozenia *)
  | Antiinterval (a , b), Antiinterval (c , d) -> 
      if (in_wartosc w 0. || in_wartosc v 0.) then 
        Interval (neg_infinity, infinity) 
      else Antiinterval ((max (a *. d) (b *. c)), (min (a *. c) (b *. d))) 



(* przedzial odwrotny do w, (1/w) *)
let odwrotnosc w=
  match w with
  | Interval (0., 0.)    -> Interval (nan , nan)
  | Interval (0., b)     -> Interval (1. /. b, infinity)
  | Interval (a, 0.)     -> Interval (neg_infinity, 1. /. a)
  | Interval (a, b)      ->
      if in_wartosc w 0. then Antiinterval (1. /. a, 1. /. b)     
      else sortpara (1. /. a)  (1. /. b)
      
  | Antiinterval (0., b) -> Interval (neg_infinity, 1. /. b)
  | Antiinterval (a, 0.) -> Interval (1. /. a, infinity )                                                   
  | Antiinterval (a, b)  -> 
      if not (in_wartosc w 0.) then Interval (1. /. a, 1. /. b)
      else Antiinterval (1. /. b, 1. /. a);;
 
(* w/v = w * (1/w) - mnozymy przez odwrotnosc *)
let podzielic w v=
  razy w (odwrotnosc v)
