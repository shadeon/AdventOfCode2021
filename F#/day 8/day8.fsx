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

let isLength length =
    String.length
    >> (=) length

let remainingCount pattern mask =
    Seq.except mask pattern
    |> Seq.length

let byDistinct length remaining pattern mask  =
    match ((isLength length) mask, remainingCount pattern mask ) with
    | ( true, r ) when r = remaining -> true
    | _ -> false

let isThree = byDistinct 5 0

let isTwo = byDistinct 5 2

let isSix = byDistinct 6 1

let getDigit connections =
    let letters = 
        Seq.sort connections
        |> List.ofSeq
    match letters with
    | [ "a"; "b"; "c";      "e"; "f"; "g" ] -> Some "0"
    | [           "c";           "f";     ] -> Some "1"
    | [ "a";      "c"; "d"; "e";      "g" ] -> Some "2"
    | [ "a";      "c"; "d";      "f"; "g" ] -> Some "3"
    | [      "b"; "c"; "d";      "f";     ] -> Some "4"
    | [ "a"; "b";      "d";      "f"; "g" ] -> Some "5"
    | [ "a"; "b";      "d"; "e"; "f"; "g" ] -> Some "6"
    | [ "a";      "c";           "f";     ] -> Some "7"
    | [ "a"; "b"; "c"; "d"; "e"; "f"; "g" ] -> Some "8"
    | [ "a"; "b"; "c"; "d";      "f"; "g" ] -> Some "9"
    | _ -> None

let getDisplay digits =
    match Seq.exists Option.isNone digits with
    | true -> None
    | false -> String.Join("", (Seq.map Option.get digits)) |> int |> Some

let getChar pattern mask =
    Seq.except mask pattern
    |> Seq.head

let decode entry =
    let patterns = getSignals entry.Patterns
    let display = getSignals entry.Display

    // first get unique values
    let one = Array.find (isLength 2) patterns
    let four = Array.find (isLength 4) patterns
    let seven = Array.find (isLength 3) patterns

    // Now get derived values that we need
    let three = Array.find (isThree one) patterns
    let six = Array.find (isSix one) patterns
    let two = Array.find (isTwo four) patterns

    // Can now build each char
    let g = "g", getChar three (Seq.append seven four)
    let connections = [
        g 
        ("a", getChar seven one)
        "b", getChar four three
        "c", getChar four six
        "d", getChar three (Seq.append seven [snd g]) 
        "e", getChar two three
        "f", getChar three two
    ]

    let decoder s = 
        Seq.find (fun (_, v) -> v = s  ) connections
        |> fst

    // can now replace each letter
    display
    |> Seq.map (fun s -> Seq.map decoder s)
    |> Seq.map getDigit
    |> getDisplay

let part2 = 
    input
    |> Seq.choose getEntry
    |> Seq.map decode
    |> Seq.choose (fun i -> i)
    |> Seq.sum

printfn "Part 2 output: %A" part2
