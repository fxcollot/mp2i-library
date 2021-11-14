let rec recherche l e = match l with
    | [] -> false
    | r::q when r=e -> true
    | r::q -> recherche q e;;
    
let inverse l =    
    let rec aux l acc = match l with
        | [] -> acc
        | e::q -> let accumulateur = e::acc in aux q accumulateur in
    aux l [];;
    
let rec taille l = match l with
    | [] -> 0
    | e::q -> 1 + taille q;;

let rec somme l = match l with
    | [] -> 0
    | e::q -> e + somme q;;
    
let rec maximum l = match l with
    | [] -> 0
    | e::q -> max e (maximum q);;
    
let rec split l = match l with
    | [] -> [], []
    | [e] -> [e], []
    | e1::e2::q -> let l1, l2 = split q in
                   e1::l1, e2::l2;;
                   
let rec concat l1 l2 = match l1, l2 with 
    | [], [] -> []
    | _, [] -> l1
    | [], _ -> l2
    | e1::q1, e2::q2 when e1 > e2 -> e2::(concat l1 q2)
    | e1::q1, e2::q2 -> e1::(concat q1 l2);;
