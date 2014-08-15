#light
module Basics
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

    let base64Chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
    
    let bytesToBase64String (data: byte list) =
        let rec convertBlock (data: byte list) =
            match data with
            | b1 :: b2 :: b3 ::  rest ->
                List.append [
                    (b1 >>> 2);                                         // highest 6 bits of b1
                    ((b1 &&& 0b11uy) <<< 4) + (b2 >>> 4);               // lowest 2 bits of b1 + highest 4 bits of b2
                    ((b2 &&& 0b1111uy) <<< 2) + ((b3 &&& 0b11000000uy) >>> 6); // lowest 4 bits of b2 + highest 2 bits of b3
                    (b3 &&& 0b111111uy);                                // lowest 6 bits of b3
                ]  (convertBlock rest)
            | b1 :: b2 :: rest ->
                List.append [
                    (b1 >>> 2);              // first 6 bits of b1
                    (b1 >>> 6) + ((b2 &&& 0b11110000uy) <<< 2); // last 2 bits of b1 + first 4 of b2
                    (b2 >>> 4);                         // last 4 bits of b2
                ]  (convertBlock rest)
            | b1 ::  rest ->
                [
                    (b1 &&& 0b11111100uy);              // first 6 bits of b1
                    (b1 >>> 6);                         // last 2 bits of b1
                ] 
            | [] -> []
        
        convertBlock data
            |> List.map (fun b -> base64Chars.[(int b)])

    [<EntryPoint>]
    let main argv =        
        let bytes = hexStringToBytes "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"
        printf "bytes: "
        Seq.iter (fun b -> printf "%x " b) bytes 
        printfn ""

        let base64String = bytesToBase64String bytes
        printf "base64: "
        Seq.iter (fun b -> printf "%c" b) base64String 
        //printfn "%s" base64String
        printfn ""

        0 // return an integer exit code
