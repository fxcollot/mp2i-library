type ('k, 'v) hashtable = { t: ('k * 'v) option array; h: 'k -> int };;

let add ht (k, v) =
    ht.t.(ht.h k) <- Some v;;

let get ht k =
    ht.t.(ht.h k);;
    
let del ht k =
    ht.t.(ht.h k) <- None;;
