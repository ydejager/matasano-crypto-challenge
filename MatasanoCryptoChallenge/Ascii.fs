module Ascii
    let bytesToAscii (bs: byte seq) =
        bs |> Seq.map char

    let asciiToBytes (cs: char seq) =
        cs |> Seq.map byte

    let textScorer cs =
        let scoreChar c =
            match c with
            | c when c >= 'a' && c <= 'z' -> 4
            | ' ' -> 3
            | c when c >= 'A' && c <= 'Z' -> 2
            | '\'' -> 1
            | _ -> 0
        (cs, Seq.fold (fun acc c -> acc + (scoreChar c)) 0 cs)

