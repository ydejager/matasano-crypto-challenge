module Base64Tests
open NUnit.Framework
open FsUnit
open Base64
open System

[<Test>]
let ``Example from wikipedia http://nl.wikipedia.org/wiki/Base64``() =
    let example = [0b11001111uy; 0b10000000uy; 0b00100000uy; 0b11100010uy; 0b10001000uy; 0b10001000uy; 0b00100000uy; 0b11100010uy; 0b10000100uy; 0b10011101uy]
    let base64 = bytesToBase64String example
    base64 |> should equal "z4Ag4oiIIOKEnQ=="


[<Test>]
let ``bytesToBase64String for xamples from wikipedia http://en.wikipedia.org/wiki/Base64``() =
    let examples = [
        "any carnal pleasure.", "YW55IGNhcm5hbCBwbGVhc3VyZS4=";
        "any carnal pleasure" , "YW55IGNhcm5hbCBwbGVhc3VyZQ==";
        "any carnal pleasur"  , "YW55IGNhcm5hbCBwbGVhc3Vy";
        "any carnal pleasu"   , "YW55IGNhcm5hbCBwbGVhc3U=";
        "any carnal pleas"    , "YW55IGNhcm5hbCBwbGVhcw==";
        "pleasure.", "cGxlYXN1cmUu";
        "leasure." , "bGVhc3VyZS4=";
        "easure."  , "ZWFzdXJlLg==";
        "asure."   , "YXN1cmUu";
        "sure."    , "c3VyZS4="]

    examples
        |> Seq.map  (fun (s,    b64) -> Ascii.asciiToBytes s, b64)
        |> Seq.map  (fun (bs,   b64) -> List.ofSeq bs, b64)
        |> Seq.map  (fun (bs,   b64) -> bytesToBase64String bs, b64)
        |> Seq.iter (fun (b64', b64) -> b64' |> should equal b64)


[<Test>]
let ``base64StringToBytes for examples from wikipedia http://en.wikipedia.org/wiki/Base64``() =
    let examples = [
        "any carnal pleasure.", "YW55IGNhcm5hbCBwbGVhc3VyZS4=";
        "any carnal pleasure" , "YW55IGNhcm5hbCBwbGVhc3VyZQ==";
        "any carnal pleasur"  , "YW55IGNhcm5hbCBwbGVhc3Vy";
        "any carnal pleasu"   , "YW55IGNhcm5hbCBwbGVhc3U=";
        "any carnal pleas"    , "YW55IGNhcm5hbCBwbGVhcw==";
        "pleasure.", "cGxlYXN1cmUu";
        "leasure." , "bGVhc3VyZS4=";
        "easure."  , "ZWFzdXJlLg==";
        "asure."   , "YXN1cmUu";
        "sure."    , "c3VyZS4="]

    examples
        |> Seq.map  (fun (s, b64) -> s, base64StringToBytes b64)
        |> Seq.map  (fun (s, bs)  -> s, Ascii.bytesToAscii bs)
        |> Seq.map  (fun (s, cs)  -> s, String.Concat cs)
        |> Seq.iter (fun (s, s')  -> s' |> should equal s)


[<Test>]
let ``Base64 with exact 6-bit blocks to bytes to Base64 should be equal``() =
    let original = "z4Ag4oiIIOKE"
    
    let bytes = base64StringToBytes original
    let output = bytesToBase64String bytes
    output |> should equal original


[<Test>]
let ``Base64 with 2 zero pads to bytes to Base64 should be equal``() =
    let original = "z4Ag4oiIIOKEnQ=="
    
    let bytes = base64StringToBytes original
    let output = bytesToBase64String bytes
    output |> should equal original
