module Ascii
    open System

    let bytesToAscii (bs: byte seq) =
        bs |> Seq.map char

    let asciiToBytes (cs: char seq) =
        cs |> Seq.map byte

    let textScorer (cs: char seq) =
        let scoreChar c =
            match c with
            | c when c >= 'a' && c <= 'z' -> 4
            | ' ' -> 3
            | c when c >= 'A' && c <= 'Z' -> 2
            | ' ' -> 2
            | '.' -> 2
            | '\'' -> 1
            | '!' -> 1
            | '!' -> 1
            | _ -> 0

        let scoreWords (cs: char seq) =
            let words = ["the"; " and "; "if"; "as"; "in"; "was"; "it"; "of"; "i"]
            let s =
                cs
                    |> Seq.map (fun c -> if Char.IsLetter(c) then Char.ToLower(c) else ' ')
                    |> String.Concat
            words
                |> Seq.map (fun w -> " " + w + " ")
                |> Seq.map (fun w -> if s.Contains(w) then 25 else 0)
                |> Seq.sum

        (Seq.fold (fun acc c -> acc + (scoreChar c)) 0 cs) + (scoreWords cs)

