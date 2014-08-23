module Challenges

open NUnit.Framework
open FsUnit
open Hex
open Base64
open Xor
open System
open Buffer
open Ascii

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

    let xoredBytes = xor keyBytes inputBytes

    let xored = bytesToHexString xoredBytes
    xored |> should equal "746865206b696420646f6e277420706c6179"


[<Test>]
let ``Challenge 3``() =
    let input = "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"
    let inputBytes = hexStringToBytes input

    inputBytes.Length * 2 |> should equal input.Length

    let ((bestChars, score), key) = Seq.head (Crack.crackXoredText 1 inputBytes)
    let s = String.Concat(bestChars)
    printf "Best bet(score %i, key: %A) %s" score key s
    printfn ""

[<Test>]
let ``Challenge 4``() =
    let lines = IO.File.ReadAllLines("4.txt")
    lines.Length |> should greaterThan 0

    let bestLines = 
        lines
            |> Seq.map hexStringToBytes
            |> Seq.map (fun (bs: byte list) -> Seq.head (Crack.crackXoredText 1 bs))
            |> Seq.map (fun ((c, s), key) -> (String.Concat c, s), key)
            |> Seq.where (fun ((_, s), _) -> s > 100)
            |> Seq.sortBy (fun ((_, s), _) -> -s)
            |> List.ofSeq
   
    bestLines.Length |> should greaterThan 0

    let ((line, score), key) = bestLines.[0]
    printfn "Best bet(score %i, key: %A): %s" score key line

    
[<Test>]
let ``Challenge 5``() =
    let key = asciiToBytes "ICE"

    let input = "Burning 'em, if you ain't quick and nimble\nI go crazy when I hear a cymbal"
    printfn "input: %s" input

    let encrypted = encryptString key input
    printfn "Crypted: %s" (encrypted |> bytesToHexString |> String.Concat)
    encrypted |> bytesToHexString |> should equal "0b3637272a2b2e63622c2e69692a23693a2a3c6324202d623d63343c2a26226324272765272a282b2f20430a652e2c652a3124333a653e2b2027630c692b20283165286326302e27282f"

    let decrypted = decryptString key encrypted
    printfn "Decrypted: %s" decrypted
    decrypted |> should equal input


[<Test>]
let ``Challenge 6``() =
    let lines = IO.File.ReadAllLines("6.txt")

    let data = lines |> Seq.map base64StringToBytes |> Seq.concat |> Array.ofSeq
    data.Length |> should greaterThan 0

    let keyLen = Crack.guesKeyLength 10 2 5 data
    printfn "Probable key length: %i" keyLen


    let key = Crack.guessRepeatedXorKey keyLen (Seq.take 200 data)
    let decrypted = xor key data
    let outputAscii = (bytesToAscii decrypted) |> Array.ofSeq
    printf "text: %s" (String.Concat outputAscii)