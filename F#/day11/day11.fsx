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

let getAdjacent (row, col) =
    let rowMin = max (row - 1) 0
    let rowMax = min (row + 1) 9
    let colMin = max (col - 1) 0
    let colMax = max (col + 1) 0

    seq { for i in rowMin .. rowMax do
            for j in colMin .. colMax -> (i, j) }
    |> Seq.where (fun (r, c) -> not <| (r = row && c = col) )

type Octopus = (int * int) * int

let octoMap =
    let newOcto row col value = Octopus ((row, col), (string >> int) value)
    
    input
    |> Seq.mapi (fun row sr -> Seq.mapi (newOcto row) sr)
    |> Seq.concat
    |> Map.ofSeq 

let isOver9000 _ value = value <> 0 && value > 9

let rec flash octopus cave count =
    let increase = 
        Option.map (fun v -> if v = 0 then 0 else v + 1)

    let increaseNearby map = 
        getAdjacent octopus
        |> Seq.fold (fun s adjOcto -> Map.change adjOcto increase s) map

    // first flash our octopus, then process nearby
    let newCave = 
        cave
        |> Map.change octopus (Option.map (fun _ -> 0))
        |> increaseNearby
            
    // see if any further octopus can now flash
    match Map.tryFindKey isOver9000 newCave with
    | None -> newCave, count + 1
    | Some o -> flash o newCave (count + 1)

let step cave =
    // power up the cave
    let poweredUp = 
        cave
        |> Map.map (fun _ v -> v + 1)
    
    match Map.tryFindKey isOver9000 poweredUp with
    | None -> poweredUp, 0
    | Some octo -> flash octo poweredUp 0

let runSteps times cave =
    let rec steps t (cave, count)=
        match t with
        | 0 -> (cave, count)
        | _ -> 
            step cave 
            |> fun (m, c) -> (m, c + count)
            |> steps (t - 1)
    
    steps times (cave, 0)

let part1 = 
    octoMap
    |> runSteps 100
    |> snd

printfn "Flash Count: %i" part1

// part 2 function

let findSync cave =
    let rec steps count cave =
        match step cave with
        | (_, 100) -> count
        | (cave, _) -> steps (count + 1) cave
    
    steps 1 cave

let part2 = findSync octoMap

printfn "Number of steps until sync: %i" part2

