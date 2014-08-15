module Console
    open Hex
    open Base64

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
