module Crack
    open Ascii
    open Xor

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

    let crack (crypter: byte seq -> byte seq -> byte seq) (scorer: char seq -> (char seq * int)) (maxkeylen: int) (input: byte seq) =
        keys maxkeylen
            |> Seq.map (fun key -> crypter key input, key)
            |> Seq.map (fun (bs, key) -> bytesToAscii bs, key)
            |> Seq.map (fun (cs, key) -> scorer cs, key)
            |> Seq.sortBy (fun ((_, i), _) -> -i)
            |> Seq.head
            
    let crackText (crypter: byte seq -> byte seq -> byte seq) (maxkeylen: int) (input: byte seq) = crack crypter textScorer maxkeylen input

    let crackXoredText (maxkeylen: int) (input: byte seq) = crackText xor maxkeylen input

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

    let guesKeyLength max depth sampleSize (data: byte seq) =
        let avgDistance keyLength sampleSize =
            // divide into chunks of equal length determined by the supplied keyLength
            let chunks = Buffer.batch keyLength data

            // Create 'pairs' of the chunks by windowing over them
            let chunkGroups = Seq.windowed depth chunks

            let rec distances (sq: byte[] seq) =
                seq {
                    let l = Seq.head sq
                    let rest = Seq.skip 1 sq
                    if not (Seq.isEmpty rest) then
                        let r = Seq.head rest

                        yield (hammingDist l r) / l.Length
                        yield! distances rest
                }

            let chunkGroupSamples = chunkGroups |> Seq.take sampleSize
            let chunkGroupDistances = chunkGroupSamples |> Seq.map (fun chunkGroup -> distances chunkGroup)
            let chunkGroupAvgDistance = chunkGroupDistances |> Seq.map (fun ds -> Seq.sum ds / depth)
            (chunkGroupAvgDistance |> Seq.sum) / sampleSize
            
        let dists = seq {
            for keyLen in [1..max] do 
                yield (keyLen, avgDistance keyLen sampleSize)
        }

        // return lowest dist
        dists 
            |> Seq.sortBy (fun (_, dist) -> dist)
            |> Seq.map fst
            |> Seq.head
        