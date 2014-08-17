module Challenges

open NUnit.Framework
open FsUnit
open Hex
open Base64
open Xor
open System
open Buffer

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


[<Test>]
let ``Challenge 3``() =
    let input = "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"
    let inputBytes = hexStringToBytes input

    inputBytes.Length * 2 |> should equal input.Length

    let (bestChars, score) = crack inputBytes
    let s = String.Concat(bestChars)
    printfn "Best bet(score %i): %s" score s

[<Test>]
let ``Challenge 5``() =
    let key = asciiToBytes "ICE"

    let input = "Burning 'em, if you ain't quick and nimble\nI go crazy when I hear a cymbal"
    printfn "input: %s" input

    let encrypted = encrypt input key
    printfn "Crypted: %s" (encrypted |> bytesToHexString |> String.Concat)
    encrypted |> bytesToHexString |> should equal "0b3637272a2b2e63622c2e69692a23693a2a3c6324202d623d63343c2a26226324272765272a282b2f20430a652e2c652a3124333a653e2b2027630c692b20283165286326302e27282f"

    let decrypted = decrypt encrypted key
    printfn "Decrypted: %s" decrypted
    decrypted |> should equal input
