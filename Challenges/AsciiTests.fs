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
let ``Char scorer``() =
    let goodText  = "This is the text that will be xor encrypted!!!!!!!"
    let badText =   "nSSHRINS_N^BOORZNMRVWY_BTH_UYICKN^^nSSHRINS_N^BOOR"

    let badScore = charScorer badText
    let goodScore = charScorer goodText

    goodScore |> should greaterThan badScore


[<Test>]
let ``Char scorer english``() =
    let goodText  = "This is the text that will be xor encrypted!!!!!!!"
    let equalText =   "Uhir ir tie uexu tiat!wiml ce yor!enbryqtee!! !! !"

    let goodScore = charScorer goodText
    let equalScore = charScorer equalText

    goodScore |> should equal equalScore

   

[<Test>]
let ``Text scorer english``() =
    let goodText  = "This is the text that will be xor encrypted!!!!!!!"
    let badText =   "Uhir ir tie uexu tiat!wiml ce yor!enbryqtee!! !! !"

    let goodScore = textScorer goodText
    let badScore = textScorer badText

    goodScore |> should greaterThan badScore