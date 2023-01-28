module Tests

open Xunit
open BinTree

[<Fact>]
let ``insert_one`` () =
  let expected = Tree(Leaf, 5, Leaf)
  let actual = empty |> insert 5
  Assert.Equal(expected, actual)

[<Fact>]
let ``insert_left_leaning`` () =
  let expected = Tree(Tree(Tree(Leaf, 1, Leaf), 2, Leaf), 5, Leaf)
  let actual = empty |> insert 5 |> insert 2 |> insert 1
  Assert.Equal(expected, actual)

[<Fact>]
let ``insert_right_leaning`` () =
  let expected = Tree(Leaf, 1, Tree(Leaf, 2, Tree(Leaf, 5, Leaf)))
  let actual = empty |> insert 1 |> insert 2 |> insert 5
  Assert.Equal(expected, actual)

[<Fact>]
let ``insert_multiple`` () =
  let expected = Tree(Tree(Leaf, 3, Leaf), 5, Tree(Leaf, 7, Leaf))
  let actual = empty |> insert 5 |> insert 3 |> insert 7
  Assert.Equal(expected, actual)

[<Fact>]
let ``toList_equals_sorted`` () =
  let expected = [3; 5; 7]
  let actual = empty |> insert 5 |> insert 3 |> insert 7 |> toList
  Assert.True(List.forall2 (fun elem1 elem2 -> elem1 = elem2) expected actual)

[<Fact>]
let ``filter_isodd`` () =
  let expected = Tree(Tree(Leaf, 3, Leaf), 5, Tree(Leaf, 7, Leaf))
  let actual = empty |> insert 5 |> insert 3 |> insert 7 |> filter (fun a -> a &&& 1 = 1)
  Assert.Equal(expected, actual)

[<Fact>]
let ``filter_iszero`` () =
  let expected = empty
  let actual = empty |> insert 5 |> insert 3 |> insert 7 |> filter (fun a -> a = 0)
  Assert.Equal(expected, actual)

[<Fact>]
let ``contains_true`` () =
    let tree = Tree(Tree(Leaf, 2, Leaf), 1, Tree(Leaf, 5, Leaf))
    let contains = tree |> contains 5
    Assert.True contains

[<Fact>]
let ``contains_false`` () =
    let tree = Tree(Tree(Leaf, 2, Leaf), 1, Tree(Leaf, 7, Leaf))
    let contains = tree |> contains 5
    Assert.False contains

[<Fact>]
let ``map_multiplynumbers`` () =
    let expected = Tree(Tree(Leaf, 2, Leaf), 4, Tree(Leaf, 6, Leaf)) |> toList
    let f = Tree(Tree(Leaf, 1, Leaf), 2, Tree(Leaf, 3, Leaf)) |> map (fun x -> x * 2)
    let actual = f |> toList
    Assert.True (List.forall2 (fun a b -> a = b) expected actual)