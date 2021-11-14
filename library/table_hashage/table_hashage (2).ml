type ('k, 'v) hashtable = { t: ('k * 'v) option array; h: 'k -> int };;
(* permet d'initaialiser une table de hachage *)

let add ht (k, v) = (* permet ajouter un élément dans la table de hachage *)
    ht.t.(ht.h k) <- Some v;;

let get ht k = (* permet de trouer un élément dans la table de hachage *)
    ht.t.(ht.h k);;
    
let del ht k = (* permet de supprimer un élément dans la table de hachage *)
    ht.t.(ht.h k) <- None;;
