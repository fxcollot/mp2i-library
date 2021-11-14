let stack_empty p = 
    p = [];;
    
let stack_push p e =
    e::p;;
    
let stakc_pop p = match p with
    | [] -> failwith "Pile vide"
    | e::q -> (e,q);;
