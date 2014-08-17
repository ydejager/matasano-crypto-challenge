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