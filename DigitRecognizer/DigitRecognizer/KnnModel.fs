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
    
    let rows = 28
    let cols = 28

    // "Sliding" aggregate of pixels by k x k blocks
    let windowed k (obs: Observation) = [|
        for row in 0 .. (rows - k) do
            for col in 0 .. (cols - k) do
                let neighbors = seq {
                    for r in 0 .. (k - 1) do
                        for c in 0 .. (k - 1) do
                            yield obs.[ (row + r) * cols + (col + c) ] }
                yield Seq.sum neighbors |]
    // Aggregation of pixels by non-overlapping k x k blocks
    let squared k (obs: Observation) = [|
        for row in 0 .. k .. (rows - k) do
            for col in 0 .. k .. (cols - k) do
                let neighbors = seq {
                    for r in 0 .. (k - 1) do
                        for c in 0 .. (k - 1) do
                            yield obs.[ (row + r) * cols + (col + c) ] }
                yield Seq.sum neighbors |]

    let w3 = windowed 3
    let s2 = squared 2

    let transform (obs: Observation) = 
        w3 obs |> Array.map (float)

    let cosine v1 v2 =
        let top = Array.fold2 (fun acc x1 x2 -> acc + x1 * x2) 0. v1 v2
        let n1 = v1 |> Array.fold (fun acc x -> acc + x * x) 0. |> sqrt
        let n2 = v2 |> Array.fold (fun acc x -> acc + x * x) 0. |> sqrt
        -0.5 - 0.5 * top / (n1 * n2)

    let train (sample: Example seq) k =
        let data = 
            sample
            |> Seq.map (fun (obs, lbl) -> transform obs, lbl)
            |> Seq.toArray
        let classify (obs: Observation) =
            let simplified = transform obs
            data
            |> Array.Parallel.map(fun o -> o, cosine (fst o) simplified)
            |> Array.sortBy snd
            |> Array.toSeq
            |> Seq.take k
            |> Seq.map (fun ((_, label), _) -> label)
            |> Seq.countBy id
            |> Seq.maxBy snd
            |> fst
        classify