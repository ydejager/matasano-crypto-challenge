module Base64
    open System

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
            | b1 :: b2 :: [] ->
                [
                    (b1 >>> 2);              // first 6 bits of b1
                    (b1 >>> 6) + ((b2 &&& 0b11110000uy) <<< 2); // last 2 bits of b1 + first 4 of b2
                    (b2 >>> 4);                         // last 4 bits of b2
                ]
            | b1 ::  [] ->
                [
                    (b1 &&& 0b11111100uy);              // first 6 bits of b1
                    (b1 >>> 6);                         // last 2 bits of b1
                ] 
            | [] -> []
        
        let chars =
            convertBlock data
                |> List.map (fun b -> base64Chars.[(int b)])

        String.Concat(chars)
