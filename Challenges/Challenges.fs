module Challenges

open NUnit.Framework
open FsUnit
open Hex
open Base64
open Xor

[<Test>]
let ``Challenge 1``() =
    let input = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"
    let bytes = hexStringToBytes input
    let base64String = bytesToBase64String bytes

    base64String |> should equal "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"

[<Test>]
let ``Challenge 2``() =
    let inputBytes = hexStringToBytes "1c0111001f010100061a024b53535009181c"
    let keyBytes = hexStringToBytes "686974207468652062756c6c277320657965"

    let xoredBytes = xorAll inputBytes keyBytes

    let xored = bytesToHexString xoredBytes
    xored |> should equal "746865206b696420646f6e277420706c6179"
        