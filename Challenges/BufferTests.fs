module BufferTests
open NUnit.Framework
open FsUnit
open Buffer
open System

[<Test>]
let ``ring should repeat sequence``() =
    "bla" 
        |> ring 
        |> Seq.take 9
        |> should equal "blablabla"

[<Test>]
let ``batch should group``() =
    "blablabla" 
        |> batch 3
        |> Seq.map String.Concat
        |> List.ofSeq
        |> should equal ["bla"; "bla"; "bla"]

[<Test>]
let ``transpose should take each nth item``() =
    "blablabla" 
        |> transpose 3
        |> List.ofSeq
        |> should equal ['b'; 'b'; 'b']


[<Test>]
let ``transpose should take last``() =
    "blablabla" 
        |> Seq.skip 2
        |> transpose 3
        |> List.ofSeq
        |> should equal ['a'; 'a'; 'a']

[<Test>]
let ``transpose should not run over last``() =
    "blablabla" 
        |> Seq.skip 3
        |> transpose 3
        |> List.ofSeq
        |> should equal ['b'; 'b']