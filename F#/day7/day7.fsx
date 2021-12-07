open System.IO

// Get the input

let readlines (filePath: string) = seq {
    use sr = new StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine ()
}

let split (delim:string, s:string) =
    s.Split(delim, System.StringSplitOptions.RemoveEmptyEntries)

let input = 
    // readlines "sample.txt" 
    readlines "input.txt" 
    |> Seq.map (fun s -> split(",",s) |> Seq.map int)
    |> Seq.head

// Part 1 functions

let median input =
    let length = Seq.length input
    
    input
    |> Seq.sort
    |> Seq.item (length / 2)

let constantFuel position location = 
    abs (position - location)

let getFuel position fuel input = 
    let pos = position input

    input
    |> Seq.map (fuel pos)
    |> Seq.sum

let part1 = getFuel median constantFuel input

printfn "Fuel used in part 1: %i" part1

// part 2 functions

let advancedFuel position location =
    let distance = constantFuel position location

    distance * (distance + 1) / 2

let getPositions input = 
    let min = Seq.min input
    let max = Seq.max input

    seq { for i in min .. max -> i }

type FuelCost = { Position: int; Cost: int }

// Probably a better way of doing this, but my stats skills suck
let bruteForce fuel positions input = 
    let costs p = getFuel (fun _ -> p) fuel input 

    input
    |> positions // get all possible positions
    |> Seq.map (fun p -> { Position=p; Cost=(costs p) }) // compute costs for each position
    |> Seq.sortBy (fun { Cost = c } -> c ) // sort by cost
    |> Seq.head // find the cheapest

let part2 = bruteForce advancedFuel getPositions input

printfn "Fuel used in Part 2: %A" part2 
