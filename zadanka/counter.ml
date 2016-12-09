(* zadanie licznik *)
module type COUNTER = sig
type counter
val make : unit -> counter (* nowy licznik o wartosci 0 *)
val inc : counter -> int (* zwieksza licznik o 1 i podaje nowa wartosc *)
val reset : unit -> unit (* zeruje wszystkie liczniki *)
end

module CO : COUNTER  = struct
type counter = { war : int ref; time : int ref }
let global = ref 0
let make () = { war = ref 0; time = ref !global }
let inc c = if !(c.time) = !global then incr c.war
            else
              begin
                c.war := 1;
                c.time := !global 
	      end;
              !(c.war)

let reset () = incr global
end

