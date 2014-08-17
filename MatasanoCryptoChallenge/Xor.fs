module Xor
    open Buffer
    open Hex
    open System
    
    let xorAll l r =
        Seq.zip l r
            |> Seq.map (fun (i, x) -> i ^^^ x)

    let bytesToAscii bs =
        bs |> Seq.map char

    let asciiToBytes cs =
        cs |> Seq.map byte

    let xorWithByte key input =
         xorAll input (ring [key])

    let crack input =
        let score cs =
            let scoreChar c = if c >= 'A' && c <= 'z' then 1 else 0
            (cs, Seq.fold (fun acc c -> acc + (scoreChar c)) 0 cs)
        [0uy..255uy]
            |> Seq.map (fun key -> xorWithByte key input)
            |> Seq.map bytesToAscii
            |> Seq.map score
            |> Seq.sortBy (fun (_, i) -> -i)
            |> Seq.head
            
    let encrypt (s: string) key =
        asciiToBytes s |> xorAll (ring key)

    let decrypt crypted key =
        xorAll crypted (ring key) |> bytesToAscii |> String.Concat