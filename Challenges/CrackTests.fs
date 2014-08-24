module CrackTests

open NUnit.Framework
open FsUnit
open Crack
open Ascii
open System

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

[<Test>]
let ``Average hamming distances 2``() =
    let sqs = [ 
        [|0b00000000uy; 0b00000000uy; 0b00000000uy; 0b00000000uy|];
        [|0b00010000uy; 0b00010000uy; 0b10000100uy; 0b10010110uy|] 
    ]
    avgHammingDistances sqs
        |> should equal (Seq.ofList [2])


[<Test>]
let ``Average hamming distances of 0, 1 and 2 should be 1, 2``() =
    avgHammingDistances [ [|0uy|]; [|1uy|]; [|2uy|] ]
        |> should equal (Seq.ofList [1; 2])

[<Test>]
let ``Average sampled hamming distances of [0;1;2] should be 1 for chunkSize 1, groupCount 3, sampleCount 1``() =
    avgSampledHammingDistances 1 3 1 [0uy; 1uy; 2uy]
        |> should equal 1

[<Test>]
let ``Average sampled hamming distances of [0;0;0] should be 0 for chunkSize 1, groupCount 3, sampleCount 1``() =
    avgSampledHammingDistances 1 3 1 [0uy; 0uy; 0uy]
        |> should equal 0

[<Test>]
let ``Average sampled hamming distances of [0; 0; 0; 0; 0; 0] should be 0 for chunkSize 2, groupCount 3, sampleCount 1``() =
    avgSampledHammingDistances 2 3 1 [0uy; 0uy; 0uy; 0uy; 0uy; 0uy;]
        |> should equal 0


[<Test>]
let ``Average sampled hamming distances 1``() =
    let data = [
        0b00000000uy; 0b00000000uy; 0b00000000uy; 0b00000000uy
        0b00010000uy; 0b00010000uy; 0b10000100uy; 0b10010110uy 
    ]
    avgSampledHammingDistances 4 2 1 data
        |> should equal 2


[<Test>]
let ``Average sampled hamming distances 2``() =
    let data = [
        0b00000000uy; 0b00000000uy; 0b00000000uy; 0b00000000uy
        0b00000000uy; 0b00000000uy; 0b00000000uy; 0b00000000uy
        0b00010000uy; 0b00010000uy; 0b10000100uy; 0b10010110uy
        0b00010000uy; 0b00010000uy; 0b10000100uy; 0b10010110uy
    ]
    avgSampledHammingDistances 8 2 1 data
        |> should equal 2

[<Test>]
let ``Average sampled hamming distances 3``() =
    let data = [
        0b00000000uy; 0b00000000uy; 0b00000000uy; 0b00000000uy
        0b00010000uy; 0b00010000uy; 0b10000100uy; 0b10010110uy
        0b00000000uy; 0b00000000uy; 0b00000000uy; 0b00000000uy
        0b00010000uy; 0b00010000uy; 0b10000100uy; 0b10010110uy
    ]
    avgSampledHammingDistances 4 2 2 data
        |> should equal 2


[<Test>]
let ``Average sampled hamming distances 4``() =
    let data = [
        0b00000000uy; 0b00000000uy; 0b00000000uy; 0b00000000uy // 0
        0b00010000uy; 0b00010000uy; 0b10000100uy; 0b10010110uy // 2
        0b00000000uy; 0b00000000uy; 0b00000000uy; 0b00000000uy // 0
        0b00010000uy; 0b00010000uy; 0b10000100uy; 0b10010110uy // 2
    ]
    avgSampledHammingDistances 2 2 4 data
        |> should equal 1


[<Test>]
let ``Average sampled hamming distances 5``() =
    let data = [
        0b00000000uy; 0b00000000uy; 0b00000000uy; 0b00000000uy // 0
        0b00010000uy; 0b00010000uy; 0b10000100uy; 0b10010110uy // 2
        0b00000000uy; 0b00000000uy; 0b00000000uy; 0b00000000uy // 0
        0b00010000uy; 0b00010000uy; 0b10000100uy; 0b10010110uy // 2
    ]
    avgSampledHammingDistances 2 4 2 data
        |> should equal 1


[<Test>]
let ``Guess key length with depth 2 and samplesize 2``() =
    let original = asciiToBytes "This is the text that will be xor encrypted!!!!!!!"
    let key = [|23uy;01uy;143uy;97uy;201uy|]
    let encrypted = Xor.xor key original
    guesKeyLength key.Length 2 5 encrypted
        |> should equal key.Length


[<Test>]
let ``Crack xored singlebyte key``() =
    let original = asciiToBytes "This is the text that will be xor encrypted!!!!!!!"
    let key = [23uy]
    let encrypted = Xor.xor key original

    let (guessedKey, score) = guessRepeatedXorKey 1 encrypted
    let text = Xor.xor guessedKey encrypted |> bytesToAscii

    printfn "%s" (String.Concat text)
    guessedKey |> should equal key


[<Test>]
let ``Crack xored multibyte key``() =
    let original = asciiToBytes "This is the text that will be xor encrypted!!!!!!!"
    //let key = [|23uy;01uy;143uy;97uy;201uy|]
    let key = [|23uy;45uy|]
    let encrypted = Xor.xor key original

    let guesses = guessRepeatedXorKeys key.Length 50 encrypted

    guesses
        |> Seq.iter (fun (guessedKey, (text, score)) -> printfn "Score %i, key%A: %s" score guessedKey text)
    
    guesses
        |> Seq.exists (fun (_, (text, _)) -> text.Equals(original))
        |> should equal true
    //guessedKey |> should equal key

[<Test>]
let ``keys 1``() =
    let allKeys = List.ofSeq (keys 1)
    allKeys.Length |> should equal 256

[<Test>]
let ``keys 2``() =
    let allKeys = List.ofSeq (keys 2)
    allKeys.Length |> should equal (256 * 256)