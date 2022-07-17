open Polynome;;

type operator =
| Plus
| Prod
| Pow ;;

type tree =
| Empty
| Int of int
| X
| Node of operator * tree array;;

(*Arbre de la figure 1*)
let t1 = Node (Plus, [|Node (Prod, [|Int (123); Node (Pow, [|X; Int (1)|])|]); Int(42); Node(Pow, [|X; Int(3)|])|]) ;;
(*Un autre arbre*)
let t2 = Node (Plus, [|Node (Prod, [|Node (Pow, [|X; Int (2)|]); Node (Pow, [|X; Int (3)|])|]); Int(42); Node(Pow, [|X; Int(3)|])|]) ;;

exception ArbreVideAbr2Poly;;

let arb2poly t =
  let rec aux p t =
    match t with
    | Empty -> raise ArbreVideAbr2Poly
    | Int a -> {c=a; d=0}::p
    | X -> p
    | Node (op, l) ->
        match op with
        | Pow -> (match l.(1) with
            | Int a -> {c=1; d=a}::p
            | _ -> p)
        | Plus -> (List.fold_left poly_add [{c=0; d=0}] (List.map (aux p) (Array.to_list l)))
        | Prod -> (List.fold_left poly_prod [{c=1; d=0}] (List.map (aux p) (Array.to_list l)))

  in canonique (aux [] t);;

(*test*)

(*affiche (arb2poly t1);;
affiche (arb2poly t2);;*)

exception ListeVideExtractionAlea;;

let extraction_alea l p =
  let len = List.length l in
  if len = 0 then (l, p)
  else
    let r = (Random.self_init(); Random.int (len)) in
    let rec aux l r newL =
      match r,l with
      | _ , [] -> raise ListeVideExtractionAlea
      | 0 , p::q ->   (p, (List.rev newL) @ q)
      | _ , p::q->  aux q (r-1) (p::newL)
    in let x, l = aux l r [] in
    (l, x::p)

let gen_permutation n =
  let rec creer_liste n acc =
    if n > 0 then
      creer_liste (n-1) (n::acc)
    else acc
  in
  let rec vider_et_remplir l p =
    match l with
    |[] -> p
    |_ -> let l, p = extraction_alea l p in vider_et_remplir l p
  in vider_et_remplir (creer_liste n []) [];;

  type bst =
  | Vide
  | Noeud of int * bst * bst;;

let abr l =
  let rec inserer a x =
    match a with
    | Vide -> Noeud (x, Vide, Vide)
    | Noeud (e, g, d) -> if x > e then Noeud (e, g, (inserer d x))
        else Noeud (e, (inserer g x), d)
  in let rec aux a l =
       match l with
       | [] -> a
       | t::q -> aux (inserer a t) q
  in aux Vide l;;

let l = [4; 2; 3; 8; 1; 9; 6; 7; 5]

let generate_int min max =
  (Random.self_init();
   Random.int (max - min + 1) + min);;

let rec etiquetage a =
  (Random.self_init();
  match a with
  | Vide -> if (Random.int 2) = 0 then
        Int (generate_int (-500) 500)  else
        X
  | Noeud (e, Vide, Vide) -> if e mod 2 = 1 then
        Node (Prod, [|Int(generate_int (-200) 200); X|])
      else
        Node (Pow, [|X; Int(generate_int 0 100) |])
  | Noeud (e, g, d) -> let rand = generate_int 1 4 in
      if rand > 3 then
        Node (Prod, [|etiquetage g; etiquetage d|])
      else
        Node (Plus, [|etiquetage g; etiquetage d|])) ;;

let gen_arb t =
  let rec f t1 t2 =
    match t1, t2 with
    | Node (op1, l1), Node (op2, l2) -> if op1 = op2 then
          (Array.map aux l2)
        else
          [|aux t2|]
    | _, _ -> [|aux t2|]

  and aux t =
    match t with
    | Node (Pow, l) -> t
    | Node (op, l) -> Node (op, Array.fold_right (fun tree -> Array.append (f t tree) ) l [||])
    | X -> Node (Pow, [|X; Int(1)|])
    | _ -> t
  in aux t;;

  (*test*)
(*  let a = etiquetage (abr [4; 2; 3; 8; 1; 9; 6; 7; 5]) ;;
  let arb = gen_arb a;;
  affiche (arb2poly arb);;
*)
