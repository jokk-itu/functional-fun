module Util

(* Returns the sum of a *)
let sum a =
    let rec aux c =
        function
        | 0 -> c 0
        | b -> aux (fun x -> c (x + b)) (b - 1)
    if a < 0 then invalidArg (nameof a) "Must not be less than Zero"
    else aux id a

(* Returns the factorial of a *)
let fac a =
    let rec aux c =
        function
        | 1 -> c 1
        | b -> aux (fun x -> c (x * b)) (b - 1)
    if a < 0 then invalidArg (nameof a) "Must not be less than Zero"
    else aux id a