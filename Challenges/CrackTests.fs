module CrackTests

open NUnit.Framework
open FsUnit
open Crack
open Ascii

[<Test>]
let ``Hamming distance of two zeros should be zero``() =
    hammingDist [0uy] [0uy]
        |> should equal 0


[<Test>]
let ``Hamming distance of 0 and 1 should be 1``() =
    hammingDist [0uy] [1uy]
        |> should equal 1

[<Test>]
let ``Hamming distance of 1011101 and 1001001 should be 2``() =
    hammingDist [0b1011101uy] [0b1001001uy]
        |> should equal 2

[<Test>]
let ``Hamming distance of "this is a test" and "wokka wokka!!!" should be 37``() =
    hammingDist (asciiToBytes "this is a test") (asciiToBytes "wokka wokka!!!")
        |> should equal 37
