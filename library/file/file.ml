type 'a file = { l1: 'a list; l2: 'a list} (* défintion d'une file *)

let queue_empty f =  (* premet de vider la file *)
    f.l1 = [] && f.l2 = [];;
    
let queue_add f e = (*permet d'ajouter un élément à la file *)
    { l1 = f.l1; l2 = e::f.l2 }
    
let rec queue_pop f = match f.l1 with (* permet de supprimer et afficher le premier élément de la liste *)
    | e::q -> e, {l1 = q; l2 = f.l2}
    | [] -> queue_pop {l1 = List.rev f.l2; l2 = []};;
