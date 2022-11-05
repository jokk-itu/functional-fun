module BinTree

type T<'a> =
    | Leaf
    | Tree of T<'a> * 'a * T<'a>

(*
    Returns an empty tree
*)
let empty = Leaf

(*
    Returns a new tree with the new element
*)
let rec insert v =
    function
    | Leaf -> Tree(Leaf, v, Leaf)
    | Tree (left, v', right) ->
        if v' > v then
            Tree(insert v left, v', right)
        else
            Tree(left, v', insert v right)

(*
    Returns the tree as a sorted list
*)
let toList t =
    let rec aux t' acc =
        match t' with
        | Leaf -> acc
        | Tree(left, v, right)
               -> acc |> aux right |> (fun x -> aux left (v::x))
    aux t List.empty

(*
    Returns the list as a tree
*) 
let fromList l = l |> List.fold (fun t x -> insert x t) empty

(*
    Returns a tree which has been filtered by 
*)
let filter f t =
    let rec aux t' acc =
        match t' with
        | Leaf -> acc
        | Tree(left, v, right) when f v
               -> acc |> insert v |> aux left |> aux right
        | Tree(left, _, right)
               -> acc |> aux left |> aux right
    aux t empty

(*
    Returns a boolean indicating whether the element is present in the tree
*)
let rec contains v =
    function
    | Leaf -> false
    | Tree (_, v', _) when v' = v
           -> true
    | Tree (left, _, right)
           -> (contains v left) || (contains v right)

(*
    Returns a tree whose elements have been applied the function f
*)
let rec map f t =
    let rec aux t' acc =
        match t' with
        | Leaf -> acc
        | Tree(left, v, right) -> insert (f v) acc |> aux left |> aux right
    aux t empty