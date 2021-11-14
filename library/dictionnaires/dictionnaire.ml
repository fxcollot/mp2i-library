let create = 
    [];;

let add c l =
    c::l;;
    
let del k l =
    List.filter ( fun c -> fst c <> k) l;;
    
let get k l =
    List.filter ( fun c -> fst c = k) l
    |>  List.map snd;;


