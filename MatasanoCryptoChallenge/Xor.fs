module Xor
    open Hex

    let xorAll (input: byte list) (key: byte list) =
        List.zip input key
            |> List.map (fun (i, x) -> i ^^^ x)