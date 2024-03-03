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