namespace ClearLines.DigitRecognizer

open System
open System.Diagnostics

module Validation =

    // Evaluate quality of model on sample
    let quality (sample: Example seq)
                (model: Model) =
        sample
        |> Seq.map (fun (obs,lbl) -> 
             if (model(obs) = lbl) then 1. else 0.)
        |> Seq.average 
        |> printfn "Correct: %f"

    let size = 28000
    // Evaluate speed of forecast production
    let speed (sample: Example seq)
              (model: Model) =
        let timer = Stopwatch()
        timer.Start()
        let run = sample |> Seq.map (fun (obs, _) -> model(obs)) |> Seq.toList
        timer.Stop()

        printfn "Sample time: %i" timer.ElapsedMilliseconds
        let ms = (int)timer.ElapsedMilliseconds * size / (run.Length)
        let time = new TimeSpan(0,0,0,0,ms)
        printfn "Estimated time: %s" (time.ToString())