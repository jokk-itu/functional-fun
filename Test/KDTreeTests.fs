module KDTreeTests

open Xunit
open KDTree

[<Fact>]
let ``insert root`` () =
    let expected = Tree(Leaf, { X = 1; Y = 1 }, Leaf)
    let actual = empty |> insert { X = 1; Y = 1 }
    Assert.Equal(expected, actual)

[<Fact>]
let ``insert left by x`` () =
    let expected = Tree(Tree(Leaf, { X = 0; Y = 0 }, Leaf), { X = 1; Y = 0 }, Leaf)
    let actual = empty |> insert { X = 1; Y = 0 } |> insert { X = 0; Y = 0 }
    Assert.Equal(expected, actual)

[<Fact>]
let ``insert right by x`` () =
    let expected = Tree(Leaf, { X = 1; Y = 0 }, Tree(Leaf, { X = 2; Y = 0 }, Leaf))
    let actual = empty |> insert { X = 1; Y = 0 } |> insert { X = 2; Y = 0 }
    Assert.Equal(expected, actual)

[<Fact>]
let ``insert left by y`` () =
    let expected = Tree(Tree(Tree(Leaf, { X = 0; Y = 0 }, Leaf), { X = 0; Y = 1 }, Leaf), { X = 1; Y = 0 }, Leaf)
    let actual = empty |> insert { X = 1; Y = 0 } |> insert { X = 0; Y = 1 } |> insert { X = 0; Y = 0 }
    Assert.Equal(expected, actual)

[<Fact>]
let ``insert right by y`` () =
    let expected = Tree(Leaf, { X = 0; Y = 0 }, Tree(Leaf, { X = 1; Y = 0 }, Tree(Leaf, { X = 1; Y = 1 }, Leaf)))
    let actual = empty |> insert { X = 0; Y = 0 } |> insert { X = 1; Y = 0 } |> insert { X = 1; Y = 1 }
    Assert.Equal(expected, actual)