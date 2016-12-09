(* ustaw II Kolos 12/13 *)
type elem = { x : int; mutable prev : elem list };;
type lista = elem list;;

let h = { x = 7; prev = []};;
let a = { x = 6; prev = []};;
let b = { x = 5; prev = []};;
let c = { x = 4; prev = []};;
let d = { x = 3; prev = []};;
let e = { x = 2; prev = []};;
let f = { x = 1; prev = []};;

let (l : lista) = [f; e; d; c; b; a; h];;

let ustaw l = 
  let front = ref l and n = ref (List.length l) in
    while !n > 0 do
      match !front with
      | (h::t) -> h.prev <- !front; front := t; decr n
      | []  -> assert false
    done;
      let back = ref (List.rev l) and przod = ref l and n = ref ((List.length l) / 2) and 
      pom = ref [] in
        while !n > 0 do
          match !back, !przod with
	  | (h::t), (g::o) ->
            pom := h.prev;
            h.prev <- g.prev;
            g.prev <- !pom;
            decr n;
            back := t;
            przod := o
	  | [], _ -> assert false
	  | _, [] -> assert false
        done

let zwroc l =
  List.fold_right (fun el a -> (el.x)::a) l [];;

let test l = 
  List.fold_right (fun el a -> (zwroc el.prev)::a) l [];;
