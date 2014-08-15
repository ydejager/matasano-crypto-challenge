module Hex
    let letterTable = ['a', 10uy ; 'b', 11uy; 'c', 12uy; 'd', 13uy; 'e', 14uy; 'f', 15uy;] |> Map.ofList
    let nrTable = ['0', 0uy ; '1', 1uy; '2', 2uy; '3', 3uy; '4', 4uy; '5', 5uy; '6', 6uy; '7', 7uy; '8', 8uy; '9', 9uy] |> Map.ofList
   
    let hexCharToNr c =
        match c with
        | nr when c >= '0' && c <= '9' ->  nrTable.[c]
        | l  when c >= 'a' && c <= 'f' -> letterTable.[c]
        | _ -> failwith (sprintf "letter '%c' was not a valid hex nr" c)


    let hexStringToBytes s =
        let rec nrsToBytes (data : byte list) =
            match data with
            | b1 :: b2 ::  rest -> List.append [(b1 <<< 4) + b2] (nrsToBytes rest)
            | b1 :: [] -> [b1]
            | [] -> []

        Seq.map hexCharToNr s
        |> List.ofSeq
        |> nrsToBytes


