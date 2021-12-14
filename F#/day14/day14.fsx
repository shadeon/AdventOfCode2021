open System.IO
open System.Collections.Generic

// Get the input

let readlines (filePath: string) = seq {
    use sr = new StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine ()
}

let split (delim:string) (s:string) =
    s.Split(delim, System.StringSplitOptions.RemoveEmptyEntries)

let input = 
    //readlines "sample.txt" 
    readlines "input.txt" 

// Part 1 functions

let template = Seq.head input

let rules = 
    Seq.skip 2 input
    |> Seq.map (split " -> " >> fun a -> Seq.head a, Seq.last a)
    |> Seq.map (fun (rule, ins) -> (rule.Chars(0), rule.Chars(1)), ins.Chars(0) )
    |> Map.ofSeq

let insert rules rule =
    match Map.tryFind rule rules with
    | None -> None
    | Some (ins) -> Some [ ins; snd rule ]

let step rules template = 
    template
    |> Seq.pairwise
    |> Seq.map (insert rules)
    |> Seq.choose id
    |> Seq.concat
    |> Seq.append (Seq.take 1 template)

let rec performSteps rules template count =
    match count with
    | 0 -> template
    | _ -> performSteps rules (step rules template) (count - 1)

let getResult counts =
    let min = counts |> Seq.minBy snd
    let max = counts |> Seq.maxBy snd
    (snd max) - (snd min)

let part1 = 
    performSteps rules template 10
    |> Seq.countBy id
    |> getResult

printfn "After step 10: %A" part1

// part 2

let AddOrIncrement amount =
    // numbers are going to get large, int isn't going to cut it
    Option.defaultValue 0.0 >> (+) amount >> Some

let processPair rules (letterCounts, pairCounts) pair pairCount =
    let newChar = Map.find pair rules
    let firstNewPair = (fst pair, newChar)
    let secondNewPair = (newChar, snd pair)

    let newCounts = 
        letterCounts
        |> Map.change newChar (AddOrIncrement pairCount)
    
    let newPairs = 
        pairCounts
        |> Map.change firstNewPair (AddOrIncrement pairCount)
        |> Map.change secondNewPair (AddOrIncrement pairCount)

    (newCounts, newPairs)

let calculatePairs rules letterCounts pairs =
    pairs
    |> Map.fold (processPair rules) (letterCounts, Map.empty)

let runSmartSteps rules template times = 
    let rec steps rules times (letterCounts, pairs) =
        match times with
        | 0 -> letterCounts
        | _ -> calculatePairs rules letterCounts pairs |> steps rules (times - 1)

    let countByDouble sequence =
        sequence
        |> Seq.countBy id
        |> Seq.map (fun (k, v) -> (k, double v))
        |> Map.ofSeq

    (countByDouble template, Seq.pairwise template |> countByDouble)
    |> steps rules times

let part2 = 
    let result = 
        runSmartSteps rules template 40
        |> Map.toSeq

    let min = result |> Seq.minBy snd
    let max = result |> Seq.maxBy snd
    (snd max) - (snd min)

printfn "Part 2: %O" part2
