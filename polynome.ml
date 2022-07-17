type monome =  {mutable c : int ; mutable d : int}
and polynome = monome list;;

(*Exemples de monômes*)

let m1 = {c = 1; d = 0 };;
let m2 = {c = 45; d = 1 };;
let m3 = {c = 13; d = 2 };;
let m4 = {c = -13; d = 2 };;
let m5 = {c = 3; d = 3 };;
let m6 = {c = 93; d = 4 };;
let m7 = {c = 0; d = 5 };;
let m8 = {c = -2; d = 0};;
let m9 = {c = 1; d = 1};;
let m10 = {c = 8; d = 0};;
let m11 = {c = 4; d = 1};;
let m12 = {c = 2; d = 2};;
let m13 = {c = 1; d = 3};;

(*Exemples de polynômes*)
let p1 = [m2; m1; m7; m1];;
let p2 = [m6; m7; m3; m1; m5; m2; m4];;
let p3 = [m6; m7; m3; m1; m5];;
let p4 = [m8;m9];;
let p5 = [m10; m11; m12; m13]

(*Fonction d'affichage*)
let rec affiche liste =
  match liste with
    [] -> ()
  | t::[] ->Printf.printf "%d" t.c;
      Printf.printf "^%d\n" t.d
  | t::q -> (Printf.printf "%d" t.c;
             Printf.printf "^%d + " t.d;
             affiche q);;


let rec partition liste pivot =
  match liste with
    [] -> [], []
  |t::q ->
      let (l1, l2) = partition q pivot in
      if (pivot.d <> t.d) then
        if (t.d < pivot.d) then (t::l1, l2)
        else (l1, t::l2)
      else
        (pivot.c <- pivot.c + t.c;
         (l1, l2));;

let rec canonique liste =
  match liste with
    [] -> []
  |pivot::l ->
      let (l1, l2) = partition l pivot in
      if (pivot.c = 0) then
        (canonique l1) @ (canonique l2)
      else
        (canonique l1) @ (pivot::(canonique l2));;

(*Tests*)

let c1 = canonique p1;;
let c2 = canonique p2;;
let c3 = canonique p3;;
let c4 = canonique p4;;
let c5 = canonique p5;;

let rec poly_add p1 p2 =
  match p1, p2 with
  |p1, [] -> p1
  |[], p2 -> p2
  |t1::q1, t2::q2 ->
      if t1.d = t2.d then let tmp = t1.c + t2.c in
        if  tmp <> 0  then
          {c = tmp; d = t2.d} :: poly_add q1 q2
        else
          poly_add q1 q2
      else
      if t1.d > t2.d then t2 :: poly_add p1 q2
      else t1 :: poly_add q1 p2;;

(*Test*)
(* print_string "Somme : ";;
let add_c1_c3 = poly_add c1 c3;;

affiche add_c1_c3;; *)

let allonge p l =
  let len = List.length p in
  if len = l then p
  else
    let rec remplir_poly p res i =
      match p with
      | [] -> if i = l then res
          else remplir_poly p ({c=0 ; d=i}::res) (i+1)
      | t::q -> if i=t.d then remplir_poly q (t::res) (i+1)
          else remplir_poly p ({c=0 ; d=i}::res) (i+1)
    in List.rev (remplir_poly p [] 0);;

let rec deg_max p =
  match p with
    [] -> 0
  | t::[] -> t.d
  | t::q -> deg_max q;;

let egalise_longueurs p1 p2 =
  let len1 = List.length p1 and len2 = List.length p2 in
  if len1 = len2 then (p1, p2)
  else
  if len1 > len2 then
    (p1, (allonge p2 len1))
  else
    ((allonge p1 len2, p2))

let rec addKara p1 p2 =
  match p1, p2 with
  |p1, [] -> p1
  |[], p2 -> p2
  |t1::q1, t2::q2 ->
      {c = t1.c + t2.c; d = t1.d} :: addKara q1 q2

let rec subKara p1 p2 =
  match p1, p2 with
    [], [] -> []
  |t::q, [] -> p1
  |[], t::q -> {c=(-t.c); d=t.d}::(subKara p1 q)
  |t1::q1, t2::q2 ->
      {c = t1.c - t2.c; d = t1.d} :: subKara q1 q2

let decompose p k =
  let rec aux p n l1 l2 = match p with
    | [] -> ((List.rev l1),(List.rev l2))
    | t::q -> if n < k then aux q (n+1) l1 (t::l2)
        else aux q (n+1) (t::l1) l2
  in aux p 0 [] [];;

let monome_shift p d =
  let rec aux p i res =
    if i = d then
      match p with
        [] -> List.rev res
      | t::q -> aux q i ({c=t.c ; d = t.d + i}::res)
    else
      aux p (i+1) ({c=0 ; d=i}::res)
  in aux p 0 [];;

let poly_prod p1 p2 =
  let p1 = allonge p1 ((deg_max p1)+1) and p2 = allonge p2 ((deg_max p2)+1) in
  let rec aux p1 p2 =
    let p1,p2 = egalise_longueurs p1 p2 in
    let d = (List.length p1) -1 in
    let d = if d mod 2 = 1 then d+1 else d in
    if d = 0 then match p1,p2 with
      | t1::_, t2::_ -> [{c= t1.c * t2.c ; d = 0}]
      | _, _ -> []
    else
      let d2 = d/2 in
      let a,b = decompose p1 d2 in
      let c,dd = decompose p2 d2 in
      let ac = aux a c in
      let bd = aux b dd in
      let abcd = aux (addKara a b) (addKara c dd) in
      addKara(monome_shift ac d) (addKara (monome_shift  (subKara abcd (addKara ac bd)) d2) bd)
  in canonique (aux p1 p2);;

let poly_prod_naif p1 p2 =
  let rec aux p1 p2 res =
    match p1, p2 with
    | [], [] -> canonique res
    | p1, [] -> canonique (res @ p1)
    | [], p2 -> canonique res
    | t1::q1, _ -> aux q1 p2 (res @ (List.fold_left (fun acc x -> {c = x.c * t1.c; d = x.d + t1.d} :: acc) [] p2))
    in aux p1 p2 []

(*tests*)
(* print_string "Produit : ";;
affiche (poly_prod c1 c3);; *)
