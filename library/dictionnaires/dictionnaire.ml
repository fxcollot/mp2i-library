(* permet d'initialiser un dictionnaire *)
let create = 
    [];;

(* permet d'ajouter un élément au dictionnaire *)
let add c l =
    c::l;;
    
(* permet de supprimer un élément du ditionnaire *)
let del k l =
    List.filter ( fun c -> fst c <> k) l;;
   
(* permet de rechercher et renvoyer un élément selon sa clé *)
let get k l =
    List.filter ( fun c -> fst c = k) l
    |>  List.map snd;;


