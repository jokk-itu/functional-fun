module Util

let sum (n:int) : int =
    let rec aux n' c =
        match n' with
        | 1 -> c n'
        | _ -> aux (n' - 1) (fun x -> c (x + n'))
    aux n id

let fac n =
    let rec aux n' c =
        match n' with
        | 1 -> c n'
        | _ -> aux (n' - 1) (fun x -> c (x * n'))
    aux n id