module UtilTests

open Xunit
open Util

[<Fact>]
let ``sum_equals`` () =
    let expect = 15
    let actual = sum 5
    Assert.Equal(expect, actual)

[<Fact>]
let ``factorisation_equals`` () =
    let expect = 120
    let actual = fac 5
    Assert.Equal(expect, actual)

[<Fact>]
let ``insertion sort empty`` () =
    let expect = []
    let actual = insertionSort []
    Assert.Equivalent(expect, actual)

[<Fact>]
let ``insertion sort one element`` () =
    let expect = [0]
    let actual = insertionSort [0]
    Assert.Equivalent(expect, actual)

[<Fact>]
let ``insertion sort multiple elements`` () =
    let expect = [0;1;2;3;4;5;6;7;8;9]
    let actual = insertionSort [0;2;9;1;3;7;5;4;6;8]
    Assert.Equivalent(expect, actual)