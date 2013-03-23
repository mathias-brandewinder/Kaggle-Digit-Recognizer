open ClearLines.DigitRecognizer.Data
open ClearLines.DigitRecognizer.Validation

[<EntryPoint>]
let main argv = 

    let dataPath = @"..\..\..\Data\"

    let examplesFile = "train.csv"
    let evaluationFile = "test.csv"
    let submissionFile = "submission.csv"

    printfn "Loading data file"
    let examplesPath = dataPath + examplesFile
    let learningSample = load examplesPath exampleReader
    printfn "Loaded data file"

    let sampleSize = 5000
    let neighbors = 9
    let model = ClearLines.DigitRecognizer.KnnModel.train learningSample neighbors

    quality (learningSample |> Seq.skip sampleSize |> Seq.take 500) model
    speed (learningSample |> Seq.skip sampleSize |> Seq.take 50) model

    printfn "Creating submission file"

    let evaluationPath = dataPath + evaluationFile;
    let submissionPath = dataPath + submissionFile;

    create evaluationPath submissionPath model validationReader outcomeWriter

    printfn "Created submission file"
    0 // return an integer exit code