module KDTree

type IPoint<'a> =
    abstract Coordinate: int -> 'a

type TwoDimensionalPoint<'a> =
    { X: 'a; Y: 'a }
    interface IPoint<'a> with
        member x.Coordinate depth = 
            if depth &&& 1 = 0 then x.X else x.Y

type T<'a> =
    | Leaf
    | Tree of T<'a> * IPoint<'a> * T<'a>

let empty = Leaf

let insert a t =
    let rec aux a' d =
        match a' with
        | Leaf -> Tree(Leaf, a, Leaf)
        | Tree (left, b, right) ->
            let aCoordinate = a.Coordinate d
            let bCoordinate = b.Coordinate d
            if aCoordinate < bCoordinate then
                Tree(aux left (d-1), b, right)
            else Tree(left, b, aux right (d-1))

    aux t 0