﻿module BinTree

type T<'a> =
    | Leaf
    | Tree of T<'a> * 'a * T<'a>

let empty = Leaf

let rec insert v =
    function
    | Leaf -> Tree(Leaf, v, Leaf)
    | Tree (left, v', right) ->
        if v' > v then
            Tree(insert v left, v', right)
        else
            Tree(left, v', insert v right)
            
let toList t =
    let rec aux t' acc =
        match t' with
        | Leaf -> acc
        | Tree(left, v, right)
               -> acc |> aux right |> (fun x -> aux left (v::x))
    aux t List.empty
    
let fromList l = l |> List.fold (fun t x -> insert x t) empty

let filter f t =
    let rec aux t' acc =
        match t' with
        | Leaf -> acc
        | Tree(left, v, right) when f v
               -> acc |> insert v |> aux left |> aux right
        | Tree(left, _, right)
               -> acc |> aux left |> aux right
    aux t empty

let rec contains v =
    function
    | Leaf -> false
    | Tree (_, v', _) when v' = v
           -> true
    | Tree (left, _, right)
           -> (contains v left) || (contains v right)