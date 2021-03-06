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
    |> Seq.map int

// Part 1 functions

let depthIncreased (first, second) = second > first

let countIncreases =
    Seq.pairwise
    >> Seq.filter depthIncreased
    >> Seq.length

let part1 = countIncreases input

printfn "Part 1: Measurements increased %i times" part1

// Part 2 functions

let getWindows = Seq.windowed 3 >> Seq.map ( Seq.fold (+) 0 )

let part2 = 
    input
    |> getWindows
    |> countIncreases

printfn "Part 2: Measurements increased %i times" part2
