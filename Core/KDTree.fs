module KDTree

type Point<'a> = { X: 'a; Y: 'a }

type T<'a> =
    | Leaf
    | Tree of T<'a> * Point<'a> * T<'a>

let empty = Leaf

let insert a t =
    let rec aux a' d =
        match a' with
        | Leaf -> Tree(Leaf, a, Leaf)
        | Tree (left, b, right) ->
            let dimension = d &&& 1
            if dimension = 0 then
                if a.X < b.X then
                    Tree(aux left (d-1), b, right)
                else Tree(left, b, aux right (d-1))
            else
                if a.Y < b.Y then
                    Tree(aux left (d-1), b, right)
                else Tree(left, b, aux right (d-1))
    
    aux t 0

