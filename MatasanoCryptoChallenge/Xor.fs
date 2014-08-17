module Xor
    open Buffer
    open Ascii
    open System

    let xor key input =
        Seq.zip (ring key) input
            |> Seq.map (fun (i, x) -> i ^^^ x)

    let encryptString key (s: string) =
        asciiToBytes s |> xor key

    let decryptString key crypted =
        xor key crypted |> bytesToAscii |> String.Concat