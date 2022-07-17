open Polynome;;
open Arbre;;

let taille = 20;;

(*Fonction générant une liste de nb_arb arbres de taille taille*)
let gen_array_arb nb_arb taille =
  let array_arb = Array.make nb_arb Empty in
  for i = 1 to nb_arb do
    array_arb.(i-1) <- gen_arb (etiquetage (abr (gen_permutation taille)));
  done;
  array_arb;;

(*Génération du jeu de données*)
let arb_100 = gen_array_arb 100 taille;;
let arb_200 = gen_array_arb 200 taille;;
let arb_300 = gen_array_arb 300 taille;;
let arb_400 = gen_array_arb 400 taille;;
let arb_500 = gen_array_arb 500 taille;;
let arb_600 = gen_array_arb 600 taille;;
let arb_700 = gen_array_arb 700 taille;;
let arb_800 = gen_array_arb 800 taille;;
let arb_900 = gen_array_arb 900 taille;;
let arb_1000 = gen_array_arb 1000 taille;;

let arbs = [arb_100; arb_200; arb_300; arb_400; arb_500; arb_600; arb_700; arb_800; arb_900; arb_1000];;

(*Stratégie 1*)

let addition_strat1 tab =
  let len = Array.length tab and res = (arb2poly tab.(0)) in
  let rec aux tab res i =
    if i = (len - 1) then
      poly_add res (arb2poly tab.(i))
    else
      aux tab (poly_add res (arb2poly tab.(i))) (i+1)
  in
  aux tab res 1;;

(*Stratégie 2*)

let rec addition_strat2 tab =
  let len = Array.length tab in
  match len with
    | 1 -> (arb2poly tab.(0))
    | 2 -> poly_add (arb2poly tab.(0)) (arb2poly tab.(1))
    | _ -> if len mod 2 = 0 then
              let len = len/2 in
              poly_add (addition_strat2 (Array.sub tab 0 len)) (addition_strat2 (Array.sub tab len len))
          else
            let len = len/2 in
            poly_add (addition_strat2 (Array.sub tab 0 len)) (addition_strat2 (Array.sub tab len (len + 1)))

(*Stratégie 3*)

exception ListeVide;;

let trier_polys polys =
  let len = Array.length polys in
    let p = polys.(0) in
      let rec aux polys i =
        if i = (len - 1) then (polys.(i) <- p;
                              polys;)

        else if List.length polys.(i+1) > List.length p then
          (polys.(i) <- p;
          polys;)
        else
          (polys.(i) <- polys.(i+1);
          aux polys (i+1);)
    in aux polys 0;;


let addition_strat3 tab =
  let polys = Array.map arb2poly tab in
    (Array.sort (fun x y -> (List.length x) - (List.length y)) polys;
    let rec aux polys =
      let len = Array.length polys in
        match len with
          | 0 -> raise ListeVide
          | 1 -> polys.(0)
          | 2 -> poly_add polys.(0) polys.(1)
          | _ -> let somme = poly_add polys.(0) polys.(1) in
                    (polys.(1) <- somme;
                    aux (trier_polys (Array.sub polys 1 (len-1)));)
        in aux polys);;



(*Stratégie 1*)

let produit_strat1 tab =
  let len = Array.length tab and res = (arb2poly tab.(0)) in
  let rec aux tab res i =
    if i = (len - 1) then
      poly_prod res (arb2poly tab.(i))
    else
      aux tab (poly_prod res (arb2poly tab.(i))) (i+1)
  in
  aux tab res 1;;

(*Stratégie 2*)

let rec produit_strat2 tab =
  let len = Array.length tab in
  match len with
    | 1 -> (arb2poly tab.(0))
    | 2 -> poly_prod (arb2poly tab.(0)) (arb2poly tab.(1))
    | _ -> if len mod 2 = 0 then
            let len = len/2 in
            poly_prod (produit_strat2 (Array.sub tab 0 len)) (produit_strat2 (Array.sub tab len len))
          else
            let len = len/2 in
            poly_prod (produit_strat2 (Array.sub tab 0 len)) (produit_strat2 (Array.sub tab len (len + 1)))

(*Stratégie 3*)

let produit_strat3 tab =
  let polys = Array.map arb2poly tab in
    (Array.sort (fun x y -> (List.length x) - (List.length y)) polys;
    let rec aux polys =
      let len = Array.length polys in
        match len with
          | 0 -> raise ListeVide
          | 1 -> polys.(0)
          | 2 -> poly_prod polys.(0) polys.(1)
          | _ -> let somme = poly_prod polys.(0) polys.(1) in
                    (polys.(1) <- somme;
                    aux (trier_polys (Array.sub polys 1 (len-1)));)
        in aux polys);;


let fichier1 = "addition_strat1.csv";;
let fichier2 = "addition_strat2.csv";;
let fichier3 = "addition_strat3.csv";;
let fichier4 = "produit_strat1.csv";;
let fichier5 = "produit_strat2.csv";;
let fichier6 = "produit_strat3.csv";;

let ecrire_stats fonction fichier arbs n pas =
  let oc = open_out fichier in
    let rec aux arbs n pas =
    match arbs with
      | [] -> close_out oc;
      | arb :: q -> let debut = Sys.time() in
                      let _ = fonction arb in
                        let fin = Sys.time() in
                        (Printf.fprintf oc "%d;%f\n" n (fin -. debut);
                        aux q (n + pas) pas;
                        )
      in aux arbs n pas;;

(*Génération des statistiques*)

(*ecrire_stats addition_strat1 fichier1 arbs 100 100;;*)
(*ecrire_stats addition_strat2 fichier2 arbs 100 100;;*)
(*ecrire_stats addition_strat3 fichier3 arbs 100 100;;*)

(* Ne pas décommenter cette ligne, temps de calcul trop long
ecrire_stats produit_strat1 fichier4 arbs 10 5;;*)

(*ecrire_stats produit_strat2 fichier5 arbs 100 100;;*)
(*ecrire_stats produit_strat3 fichier6 arbs 100 100;;*)

let generer_arbres =
  let res = Array.make 15 Empty in
   let taille = ref 1 in
    res.(0) <- gen_arb (etiquetage (abr (gen_permutation 1)));
  for i=1 to 14 do
    res.(i) <- gen_arb (etiquetage (abr (gen_permutation !taille)));
    taille := (!taille) * 2;
  done;
  res;;

(*Génération de la liste d'arbres de taille 1,1,2,4,...,2^13*)
let arbs2 = generer_arbres;;

let ecrire_stats2 fonction arbs =
  let debut = Sys.time () in
    let _ = fonction arbs in
      let fin = Sys.time () in
        Printf.printf "%f " (fin -. debut) ;;

(*Génération des statistiques*)

(*ecrire_stats2 addition_strat1 arbs2;;
ecrire_stats2 addition_strat2 arbs2;;
ecrire_stats2 addition_strat3 arbs2;;*)

(*ecrire_stats2 produit_strat1 arbs2;;*)
(*ecrire_stats2 produit_strat2 arbs2;;
ecrire_stats2 produit_strat3 arbs2;;*)
