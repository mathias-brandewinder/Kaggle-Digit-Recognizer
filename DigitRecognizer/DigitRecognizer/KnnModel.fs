namespace ClearLines.DigitRecognizer

module KnnModel = 

    // Simple Euclidean distance
    let distance (obs1: Observation) obs2 =
        Array.fold2 (fun acc o1 o2 -> 
            acc + pown (o1 - o2) 2) 0 obs1 obs2
    // L3 distance
    let L3 (obs1: Observation) obs2 =
        Array.fold2 (fun acc o1 o2 -> 
            let x = abs (o1 - o2)
            acc + x * x * x) 0 obs1 obs2
    // Simplification of observations,
    // to avoid int overflow in L3 calculation
    let simplify (obs: Observation) =
        Array.map (fun x -> x / 10) obs

    let train (sample: Example seq) k =
        let data = 
            sample
            |> Seq.map (fun (obs, lbl) -> simplify obs, lbl)
            |> Seq.toArray
        let classify (obs: Observation) =
            let simplified = simplify obs
            data
            |> Array.Parallel.map(fun o -> o, L3 (fst o) simplified)
            |> Array.sortBy snd
            |> Array.toSeq
            |> Seq.take k
            |> Seq.map (fun ((_, label), _) -> label)
            |> Seq.countBy id
            |> Seq.maxBy snd
            |> fst
        classify