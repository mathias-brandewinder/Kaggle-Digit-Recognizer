namespace ClearLines.DigitRecognizer

module KnnModel = 

    let distance (obs1: Observation) obs2 =
        Array.fold2 (fun acc o1 o2 -> acc + pown (o1 - o2) 2) 0 obs1 obs2

    let train (sample: Example seq) k =
        let classify (obs: Observation) =
            sample
            |> Seq.sortBy (fun o -> distance (fst o) obs)
            |> Seq.take k
            |> Seq.map snd
            |> Seq.countBy id
            |> Seq.maxBy snd
            |> fst
        classify