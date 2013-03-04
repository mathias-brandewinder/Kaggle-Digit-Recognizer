namespace ClearLines.DigitRecognizer

type Observation = int []
type Outcome = Zero | One | Two | Three | Four | Five | Six | Seven | Eight | Nine
type Example = Observation * Outcome
type ExampleReader = string [] -> Example
type ValidationReader = string [] -> Observation
type OutcomeWriter = Outcome -> string
type Model = Observation -> Outcome

module Data =

    open System
    open System.IO
    open System.Text.RegularExpressions
    open Microsoft.VisualBasic.FileIO    

    let parseCsv (filePath: string) =
        use reader = new TextFieldParser(filePath)
        reader.TextFieldType <- FieldType.Delimited
        reader.SetDelimiters(",")
        [ while (not reader.EndOfData) do yield reader.ReadFields() ]

    let outcomeWriter (outcome:Outcome) = 
        match outcome with
        | Zero  -> "0"
        | One   -> "1"
        | Two   -> "2"
        | Three -> "3"
        | Four  -> "4"
        | Five  -> "5"
        | Six   -> "6"
        | Seven -> "7"
        | Eight -> "8"
        | Nine  -> "9"

    let outcomeReader (outcome: string) =
        match outcome with
        | "0" -> Zero
        | "1" -> One
        | "2" -> Two
        | "3" -> Three
        | "4" -> Four
        | "5" -> Five
        | "6" -> Six
        | "7" -> Seven
        | "8" -> Eight
        | "9" -> Nine
        | _   -> failwith "Unrecognized case"

    let exampleReader (line: string[]) =
        let label, data = line.[0], line.[1..]
        data |> Array.map (fun x -> Convert.ToInt32(x)), outcomeReader label

    let validationReader (line: string []) =
        line |> Array.map (fun x -> Convert.ToInt32(x))

    let load examplesFile
             (reader: ExampleReader) =
        parseCsv examplesFile
        |> List.tail
        |> List.map reader

    // create submission file
    let create sourceFile 
               resultFile 
               (model: Model)
               (reader: ValidationReader)
               (writer: OutcomeWriter) =
        let data = 
            parseCsv sourceFile 
            |> List.tail
            |> Seq.map reader
            |> Seq.map (fun e -> writer (model e))
            |> Seq.toArray
        File.WriteAllLines(resultFile, data)