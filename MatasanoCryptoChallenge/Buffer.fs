module Buffer
    let ring items =
        let rec iter items' =
            seq {
                if Seq.isEmpty items' then
                    yield! (iter items)
                else 
                    yield (Seq.head items')
                    yield! (iter (Seq.skip 1 items'))
            }
        iter items

    let batch size (source: seq<_>) =
      seq { let arr = Array.create size (Seq.nth 0 source)
            let i = ref 0
            use e = source.GetEnumerator()
            while e.MoveNext() do
              arr.[!i] <- e.Current
              i := !i + 1
              if !i = size then
                yield Array.init size (fun j -> arr.[j])
                i := 0
          }

    let rec transpose skip cs =
        let skipIf n s =
            s
            |> Seq.mapi (fun i elem -> i, elem)
            |> Seq.choose (fun (i, elem) -> if i >= n then Some(elem) else None)

        seq {
            yield Seq.head cs
            let rest = (skipIf skip cs)
            if not (Seq.isEmpty rest) then
                yield! transpose skip rest
        }