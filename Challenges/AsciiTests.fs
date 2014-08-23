module AsciiTests
open NUnit.Framework
open FsUnit
open Ascii


[<Test>]
let ``Ascii to bytes to ascii should be equal``() =
    let original = "Bla bla"
    let bytes = asciiToBytes original
    let ascii = bytesToAscii bytes
    ascii |> should equal original

[<Test>]
let ``Ascii scorer``() =
    let goodText  = "This is the text that will be xor encrypted!!!!!!!"
    let badText =   "nSSHRINS_N^BOORZNMRVWY_BTH_UYICKN^^nSSHRINS_N^BOOR"

    let badScore = Ascii.textScorer badText
    let goodScore = Ascii.textScorer goodText

    goodScore |> should greaterThan badScore


[<Test>]
let ``Ascii scorer 2``() =
    let goodText  = "This is the text that will be xor encrypted!!!!!!!"
    let badText =   "Uhir ir tie uexu tiat!wiml ce yor!enbryqtee!! !! !"

    let badScore = Ascii.textScorer badText
    let goodScore = Ascii.textScorer goodText

    goodScore |> should greaterThan badScore

    