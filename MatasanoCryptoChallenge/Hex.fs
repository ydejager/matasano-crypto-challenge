module Hex
    open System

    let fromTable = 
        [
            'a', 10uy ; 'b', 11uy; 'c', 12uy; 'd', 13uy; 'e', 14uy; 'f', 15uy;
            'B', 10uy ; 'B', 11uy; 'C', 12uy; 'D', 13uy; 'E', 14uy; 'F', 15uy;
            '0', 0uy ; '1', 1uy; '2', 2uy; '3', 3uy; '4', 4uy; '5', 5uy; '6', 6uy; '7', 7uy; '8', 8uy; '9', 9uy;
        ] |> Map.ofList

    let toTable = 
        [
            10uy, 'a' ; 11uy, 'b'; 12uy, 'c'; 13uy, 'd'; 14uy, 'e'; 15uy, 'f';
            0uy, '0'; 1uy, '1'; 2uy, '2'; 3uy, '3'; 4uy, '4'; 5uy, '5'; 6uy, '6'; 7uy, '7'; 8uy, '8'; 9uy, '9';
        ] |> Map.ofList

      
    let hexCharToNr c =
        match c with
        | nr when c >= '0' && c <= '9' ->  fromTable.[c]
        | l  when c >= 'a' && c <= 'f' -> fromTable.[c]
        | l  when c >= 'A' && c <= 'F' -> fromTable.[c]
        | _ -> failwith (sprintf "letter '%c' was not a valid hex nr" c)


    let nrToHexChar b =
        match b with
        | nr when b >= 0uy && b <= 15uy ->  toTable.[nr]
        | _ -> failwith (sprintf "nr '%i' was not a valid hex nr" b)

    let hexStringToBytes (s: string) =
        let rec nrsToBytes (data : byte list) =
            match data with
            | b1 :: b2 ::  rest -> List.append [(b1 <<< 4) + b2] (nrsToBytes rest)
            | b1 :: [] -> [b1]
            | [] -> []
        
        let chars = s.ToCharArray()

        Seq.map hexCharToNr chars
            |> List.ofSeq
            |> nrsToBytes

    let bytesToHexString (bs : byte seq) =
        Seq.map (fun b -> [b >>> 4; b &&& 0b00001111uy]) bs
            |> Seq.concat
            |> Seq.map nrToHexChar
            |> String.Concat
        