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
let ``insertionSort_equals`` () =
    let expect = [0; 3; 7; 8; 9; 11]
    let actual = insertionSort (fun x y -> x < y) [0; 9]
    Assert.True (List.forall2 (fun x y -> x = y) expect actual)