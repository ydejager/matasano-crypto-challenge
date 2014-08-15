module Xor
    open Hex
    open System

    let xorAll (input: byte list) (key: byte list) =
        List.zip input key
            |> List.map (fun (i, x) -> i ^^^ x)

    let bytesToAscii bs =
        bs |> List.map char

    let score cs =
        let scoreChar c = if c >= 'A' && c <= 'z' then 1 else 0
        (cs, List.fold (fun acc c -> acc + (scoreChar c)) 0 cs)

    let xorWithByte (key: byte) =
         List.map (fun b -> b ^^^ key)

    let crack (input: byte list) =
        [0uy..255uy]
            |> List.map (fun key -> xorWithByte key input)
            |> List.map bytesToAscii
            |> List.map score
            |> Seq.sortBy (fun (_, i) -> -i)
            |> Seq.head
            