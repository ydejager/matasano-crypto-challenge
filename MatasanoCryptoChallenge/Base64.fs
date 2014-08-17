module Base64
    open System

    let base64Chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"    
    let base64Bytes =
        base64Chars
            |> Seq.mapi (fun i c -> (c, byte i))
            |> Map.ofSeq
        
    let bytesToBase64String (data: byte list) =
        let rec convertBlock (data: byte list) =
            match data with
            | b1 :: b2 :: b3 ::  rest ->
                List.append [
                    b1 >>> 2                                                  // highest 6 bits of b1
                    ((b1 &&& 0b11uy) <<< 4) + (b2 >>> 4)                      // lowest 2 bits of b1 + highest 4 bits of b2
                    ((b2 &&& 0b1111uy) <<< 2) + ((b3 &&& 0b11000000uy) >>> 6) // lowest 4 bits of b2 + highest 2 bits of b3
                    b3 &&& 0b111111uy                                         // lowest 6 bits of b3
                ]  (convertBlock rest)
            | b1 :: b2 :: [] ->
                [
                    b1 >>> 2                                                   // first 6 bits of b1
                    ((b1 &&& 0b11uy) <<< 4) + (b2 >>> 4)                       // lowest 2 bits of b1 + highest 4 bits of b2
                    ((b2 &&& 0b1111uy) <<< 2)                                  // lowest 4 bits of b2 + zeros
                ]
            | b1 ::  [] ->
                [
                    b1 >>> 2                                                  // highest 6 bits of b1
                    ((b1 &&& 0b11uy) <<< 4)                                   // lowest 2 bits of b1 + zeros
                ] 
            | [] -> []
        
        let chars =
            convertBlock data
                |> List.map (fun b -> base64Chars.[(int b)])

        let pad l = match (l % 3) with
                    | 1 -> ['='; '=']
                    | 2 -> ['=']
                    | _ -> []
        let padded = List.concat [chars; (pad data.Length)]
        String.Concat padded

    let base64StringToBytes (chars: char seq) =
        let rec convertBlock (data: byte list) =
            match data with
            | b1 :: b2 :: b3 :: b4 :: rest ->
                List.append [
                    (b1 <<< 2) + ((b2 &&& 0b00110000uy) >>> 4)
                    ((b2 &&& 0b00001111uy) <<< 4) + ((b3 &&& 0b00111100uy) >>> 2)
                    ((b3 &&& 0b00000011uy) <<< 6) + b4
                ] (convertBlock rest)
            | b1 :: b2 :: b3 :: [] ->
                [
                    (b1 <<< 2) + ((b2 &&& 0b00110000uy) >>> 4)
                    ((b2 &&& 0b00001111uy) <<< 4) + ((b3 &&& 0b00111100uy) >>> 2)
                ]
            | b1 :: b2 :: [] ->
                [(b1 <<< 2) + ((b2 &&& 0b00110000uy) >>> 4)]
            | [] -> []
            | _  -> failwith "Invalid Base64 string. Length should be multiple of 4"

        let byteValues =
            chars
                |> Seq.takeWhile base64Bytes.ContainsKey
                |> Seq.map (fun c -> base64Bytes.[c])
                |> List.ofSeq

        convertBlock byteValues