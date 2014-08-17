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