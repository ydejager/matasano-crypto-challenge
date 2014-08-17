module Xor
    open Buffer
    open Ascii
    open System

    let xor (key: byte seq) (input: byte seq) =
        Seq.zip (ring key) input
            |> Seq.map (fun (i, x) -> i ^^^ x)

    let encryptString (key: byte seq) (s: string) =
        asciiToBytes s |> xor key

    let decryptString (key: byte seq) (crypted: byte seq) =
        xor key crypted |> bytesToAscii |> String.Concat