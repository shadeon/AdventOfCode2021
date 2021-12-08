open System.IO
open System

// Get the input

let readlines (filePath: string) = seq {
    use sr = new StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine ()
}

let split (delim:string, s:string) =
    s.Split(delim, System.StringSplitOptions.RemoveEmptyEntries)

let input = 
    //readlines "sample.txt" 
    readlines "input.txt" 

// Part 1 functions

type Entry = { Patterns: string; Display: string }

type UniqueDigit = One | Four | Seven | Eight

let getEntry line = 
    match split("|", line) with
    | [| p; d |] -> Some { Patterns = p; Display = d }
    | _ -> None

let getSignals signals = 
    split(" ", signals)

let uniqueSignals signals =
    let cat s =
        match String.length s with
        | 2 -> Some One
        | 3 -> Some Seven
        | 4 -> Some Four
        | 7 -> Some Eight
        | _ -> None
    
    Seq.choose cat signals

let part1 = 
    input
    |> Seq.choose getEntry
    |> Seq.map (fun { Display = d; Patterns = _ } -> getSignals d )
    |> Seq.map uniqueSignals
    |> Seq.sumBy Seq.length

printfn "Number of unique digits on displays: %i" part1

// part 2

let isLength length = String.length >> (=) length

let remainingDifference remaining pattern mask =
    Seq.except mask pattern
    |> Seq.length
    |> (=) remaining
    

let getDisplay digits =
    match Seq.exists Option.isNone digits with
    | true -> None
    | false -> 
        String.Join("", (Seq.map Option.get digits))
        |> int 
        |> Some

let (|Digit|_|) one four digit =
    let length = String.length digit

    let isThree = remainingDifference 0 one
    let isTwo = remainingDifference 2 four
    let isSix = remainingDifference 1 one
    let isNine pattern = remainingDifference 2 pattern four

    match length with
    | 2 -> Some "1"
    | 3 -> Some "7"
    | 4 -> Some "4"
    | 7 -> Some "8"
    | 5 when isTwo digit -> Some "2"
    | 5 when isThree digit -> Some "3"
    | 5 -> Some "5"
    | 6 when isSix digit -> Some "6"
    | 6 when isNine digit -> Some "9"
    | 6 -> Some "0"
    | _ -> None

let decode entry =
    let patterns = getSignals entry.Patterns
    let display = getSignals entry.Display

    // first get unique values
    let one = Array.find (isLength 2) patterns
    let four = Array.find (isLength 4) patterns

    let decoder s = 
        match s with
        | Digit one four d -> Some d
        | _ -> None

    // can now replace each letter
    display
    |> Seq.map decoder
    |> getDisplay

let part2 = 
    input
    |> Seq.choose getEntry
    |> Seq.map decode
    |> Seq.choose (fun i -> i)
    |> Seq.sum

printfn "Part 2 output: %A" part2
