open System
open System.IO

// Get the input

let readlines (filePath: string) = seq {
    use sr = new StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine ()
}

let input = 
    //readlines "sample.txt" 
    readlines "input.txt" 

// Part 1 functions

let toDecimal input = 
    Convert.ToInt32(input, 2)

let toDigits input = 
    Seq.map (fun c -> Seq.ofList [c]) input

let join first second = 
    Seq.map2 Seq.append first second

let gamma = Seq.maxBy

let epsilon = Seq.minBy

let getDigit rate input =
    rate (fun (_, c) -> c) input
    |> fst

let getCounts input = 
    input
    |> Seq.map toDigits
    |> Seq.reduce join
    |> Seq.map (Seq.countBy (fun d -> d))

let getRate rate counts = 
    Seq.map (getDigit rate) counts
    |> Seq.map string // char to string
    |> String.Concat // join them together
    |> toDecimal

let part1 =
    let counts = getCounts input
    let epRate = getRate epsilon counts
    let gammaRate = getRate gamma counts
    epRate * gammaRate

printfn "Part 1 Result: %i" part1 

// Part 2 functions

let rec getRecursiveRate rate index input = 
    let digit =
        input 
        |> Seq.map (fun (s: string) -> s.Substring(index, 1) )
        |> getCounts
        |> Seq.map (getDigit rate)
        |> Seq.map string
        |> Seq.head

    let matches = input |> Seq.where (fun (s: string) -> s.Substring(index, 1) = digit)

    match Seq.length matches with
        | 1 ->  Seq.head matches |> toDecimal
        | _ -> getRecursiveRate rate (index + 1) matches

let oxygenRating projection = Seq.sortByDescending fst >> Seq.maxBy projection

let co2ScrubberRating projection = Seq.sortBy fst >> Seq.minBy projection

let part2 = 
    let o2Rate = getRecursiveRate oxygenRating 0 input
    let co2Rate = getRecursiveRate co2ScrubberRating 0 input
    o2Rate * co2Rate

printfn "Part 2 Result: %i" part2
