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
