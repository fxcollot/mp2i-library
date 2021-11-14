let rec concat l1 l2 = match l1 with
    | [] -> l2
    | e::q -> e::(concat q l2);;
    
let rec partition pivot l = match l with
    | [] -> ([], [])
    | e::q -> let l1, l2 = (partition pivot q) in
                if e < pivot then ((e::l1), l2)
                else (l1, (e::l2));;
                    
let rec tri_rapide l = match l with
    | [] -> []
    | [e] -> [e]
    | e::q -> let l1, l2 = partition e q in
        concat (tri_rapide l1) (e::tri_rapide l2);;

let rec split l = match l with
    | [] -> [], []
    | [e] -> [e], []
    | e1::e2::q -> let q1, q2 = split q in
        e1::q1, e2::q2;;
        
let rec fusion l1 l2 = match l1, l2 with
    | [], _ -> l2
    | _, [] -> l1
    | e1::q1, e2::q2 when e1 < e2 -> e1::fusion q1 l2
    | e1::q1, e2::q2 -> e2::fusion l1 q2;;

let rec tri_fusion l = match l with
    | [] -> []
    | [e] -> [e]
    | l -> let l1, l2 = split l in
    fusion (tri_fusion l1) (tri_fusion l2);;

