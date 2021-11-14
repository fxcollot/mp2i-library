let create = (* permet d'initialiser une pile *)
    [];;

let stack_empty p = (* permet de vider une pile *)
    p = [];;
    
let stack_push p e = (* permet de rajouter un élément dans une pile *)
    e::p;;
    
let stack_pop p = match p with (* permet d'afficher le premier élément de la liste (dernier ajouté)*)
    | [] -> failwith "Pile vide"
    | e::q -> (e,q);;
