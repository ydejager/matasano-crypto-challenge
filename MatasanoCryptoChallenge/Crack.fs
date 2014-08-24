module Crack
    open Ascii
    open Xor
    open System

    let keys l =    
        let prefixbyte suffix =
            seq { for b in 0uy .. 255uy do yield b :: suffix }

        let rec g l =
            seq {            
                match l with
                | 0 -> failwith "Length must be larger than zero"
                | 1 -> yield! prefixbyte []
                | n -> for bs in g (n - 1) do yield! prefixbyte bs
            }
        g l

    let crack (crypter: byte seq -> byte seq -> byte seq) (scorer: char seq -> int) (keylen: int) (input: byte seq) =
        keys keylen
            |> Seq.map (fun key -> crypter key input, key)
            |> Seq.map (fun (bs, key) -> bytesToAscii bs, key)
            |> Seq.map (fun (cs, key) -> (cs, scorer cs), key)
            |> Seq.sortBy (fun ((_, i), _) -> -i)
            //|> Seq.head
            
    let crackChars (crypter: byte seq -> byte seq -> byte seq) (keylen: int) (input: byte seq) = crack crypter charScorer keylen input
    let crackText (crypter: byte seq -> byte seq -> byte seq) (keylen: int) (input: byte seq) = crack crypter textScorer keylen input

    let crackXoredChars (keylen: int) (input: byte seq) = crackChars xor keylen input
    let crackXoredText (keylen: int) (input: byte seq) = crackText xor keylen input

    let hammingDist (bs1: byte seq) (bs2: byte seq) =
        let countBits (b: byte) =
            let rec f b =
                match b with
                | 0uy -> 0
                | b -> 1 + (f (b &&& (b - 1uy)))
            f b

        let hd (b1:byte) (b2:byte) =
            countBits (b1 ^^^ b2)

        Seq.zip bs1 bs2
            |> Seq.map (fun (b1, b2) -> hd b1 b2)
            |> Seq.sum

    let rec avgHammingDistances (sq: byte[] seq) =
        seq {
            let l = Seq.head sq
            let rest = Seq.skip 1 sq
            if not (Seq.isEmpty rest) then
                let r = Seq.head rest

                yield (hammingDist l r) / l.Length
                yield! avgHammingDistances rest
        }

    let avgSampledHammingDistances chunkSize groupCount sampleCount data =
        // divide into chunks of equal length determined by the supplied keyLength
        let chunks = Buffer.batch chunkSize data

        // Create 'groups' of the chunks by batching over them
        let chunkGroups = Buffer.batch groupCount chunks

        let chunkGroupSamples = chunkGroups |> Seq.take sampleCount |> Array.ofSeq
        let chunkGroupDistances = chunkGroupSamples |> Seq.map (fun chunkGroup -> avgHammingDistances chunkGroup) |> Array.ofSeq
        let chunkGroupAvgDistance = chunkGroupDistances |> Seq.map (fun ds -> Seq.sum ds / (groupCount - 1)) |> Array.ofSeq
        (chunkGroupAvgDistance |> Seq.sum) / sampleCount

    let guesKeyLength max depth sampleSize (data: byte seq) =            
        let dists = seq {
            for keyLen in [1..max] do 
                yield (keyLen, avgSampledHammingDistances keyLen depth sampleSize data)
        }

        // return lowest dist
        dists 
            |> Seq.sortBy (fun (_, dist) -> dist)
            |> Seq.map fst
            |> Seq.head
    

    let guessRepeatedXorKeys (keyLen: int) (topN: int) (data: byte seq) =
        let guessNthByte n =        
            Seq.skip n data // start at n bytes
                |> Buffer.transpose keyLen // take each nth byte
                |> crackXoredChars 1         // attempt to crack the series of nth bytes with a 1-byte key
                |> Seq.take topN
                |> Seq.map (fun ((_, score), key) -> (key.[0], score))
        let guesses = seq {    
            for n in [0 .. keyLen - 1] do
                yield (guessNthByte n)
        }
        // TODO: combine using cartesion product
        let combined =
            guesses
                |> List.ofSeq
                //|> Buffer.zipseq 
                |> Buffer.productN
                |> Seq.map (fun es -> Seq.fold (fun (l, total) (key, score) -> (List.append l [key], total + score)) ([], 0) es)
                //|> Seq.map (fun (key, score) -> (List.rev key, score))

        combined
            |> Seq.map (fun (key, score) -> 
                    let decrypted = xor key data |> bytesToAscii
                    (key, (String.Concat decrypted, score + (Ascii.textScorer decrypted)))
                )
            |> Seq.sortBy (fun (key, (text, score)) -> -score)

    let guessRepeatedXorKey keyLen data =
        Seq.head (guessRepeatedXorKeys keyLen 1 data)
