let pref t =
let p = Array.make (Array.length(t) + 1) 0 and pj = ref 0 in
  begin
    for i = 2 to Array.length t do
      while (!pj > 0) && (t.(!pj) <> t.(i - 1)) do
        pj := p.(!pj)
      done;
      if t.(!pj) = t.(i - 1) then pj := !pj + 1;
      p.(i) <- !pj
    done;
    p
  end;;

let find x y =
  let
   i = ref 0 and
   j = ref 0 and
   w = ref [] and
   p = pref x
   in
   while !i <= Array.length y - Array.length x do
     j := p.(!j);
     while (!j < Array.length x) && (x.(!j) = y.(!i + !j)) do
       j := !j + 1
     done;
     if !j = Array.length x  then w := !i::!w;
     i := !i + if !j >0 then !j - p.(!j) else 1
   done;
   List.rev !w;;

find [|'a'|] [|'a'; 'b'; 'a'; 'c'|];;
