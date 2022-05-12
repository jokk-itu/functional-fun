open System

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

[<EntryPoint>]
let main argv =
    printf $"The sum of 5 is %i{sum 5} \n"
    printf $"The factorial of 5 is %i{fac 5} \n"
    0