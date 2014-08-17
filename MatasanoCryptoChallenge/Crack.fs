module Crack
    open Ascii
    open Xor
    
    let crack crypter scorer input =
        [0uy..255uy]
            |> Seq.map (fun key -> crypter [key] input)
            |> Seq.map bytesToAscii
            |> Seq.map scorer
            |> Seq.sortBy (fun (_, i) -> -i)
            |> Seq.head
            
    let crackText crypter input = crack crypter textScorer input

    let crackXoredText input = crackText xor input
