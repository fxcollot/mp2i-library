let rec recherche l e = match l with (* permet de rechercher un élément dans une liste *)
    | [] -> false
    | r::q when r=e -> true
    | r::q -> recherche q e;;
    
let inverse l =    (* permet d'inverser une liste *)
    let rec aux l acc = match l with
        | [] -> acc
        | e::q -> let accumulateur = e::acc in aux q accumulateur in
    aux l [];;
    
let rec taille l = match l with (* permet de calculer la taille d'une liste *)
    | [] -> 0
    | e::q -> 1 + taille q;;

let rec somme l = match l with (* permet d'effectuer la somme des éléments d'une liste *)
    | [] -> 0
    | e::q -> e + somme q;;
    
let rec maximum l = match l with (* peremet de trouver le maximum de la liste *)
    | [] -> 0
    | e::q -> max e (maximum q);;
    
let rec split l = match l with (* permet de séparer la liste en deux listes de tailles égales *)
    | [] -> [], []
    | [e] -> [e], []
    | e1::e2::q -> let l1, l2 = split q in
                   e1::l1, e2::l2;;
                   
let rec concat l1 l2 = match l1, l2 with (* permet de concataner deux listes*)
    | [], [] -> []
    | _, [] -> l1
    | [], _ -> l2
    | e1::q1, e2::q2 when e1 > e2 -> e2::(concat l1 q2)
    | e1::q1, e2::q2 -> e1::(concat q1 l2);;
