type 'a dynamique = { mutable tableau: 'a array; mutable taille : int};;
(* permet d'initialiser un tableau dynamique *)

let copy t1 t2 = (* pemret de copier un tableau t1 dans un tableau t2 *)
    for i = 0 to Array.length t1 - 1 do
        t2.(i) <- t1.(i)
    done;;
    
let add e t = (* permet d'ajouter un élément dans un tableau dynamique sans se soucier de la taille de ce dernier *)
    if t.taille < Array.length t.tableau then (t.tableau.(tableau.taille) <- e; t.taille <-t.taille + 1)
    else if t.taille = 0 then t.tableau <- [ |e| ]
    else let tinter = Array.make (2*t.taille) 0 in 
        (copy t.tableau tinter; tinter.(t.taille) <- e; t.tableau <- tinter);
    t.taille <- t.taille + 1;;
