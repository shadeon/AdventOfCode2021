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
    //readlines "sample.txt" 
    readlines "input.txt" 
    |> Seq.map (fun s -> split(",",s) |> Seq.map int)
    |> Seq.head

// Part 1 functions

let sortfish = 
    Seq.countBy (fun f -> f) 
    >> Seq.map (fun (d, c) -> d, (uint64 c)) // going to need a bigger net..

let tickFish (day, count) = day - 1, count

let addOrUpdateFish (day, count) input = 
    match Seq.exists (fun (d, _) ->  day = d ) input with
    | false -> Seq.append input (seq { day, count })
    | true -> Seq.map (fun (d, c) -> if d = day then (d, c + count) else (d,c)) input

let breedFish cycle maturity school =
    let foldFish state (day, count) =
        let fish =
            match day with
            | -1 -> cycle, count
            | _ -> day, count
        
        match day with
        | -1 -> addOrUpdateFish fish state |> addOrUpdateFish (cycle + maturity, count)
        | _ -> addOrUpdateFish fish state
        
    school
    |> Seq.fold foldFish Seq.empty

let rec simulateFish cycle maturity days input =
    match days with
    | 0 -> input
    | _ ->
        Seq.map tickFish input
        |> breedFish cycle maturity
        |> simulateFish cycle maturity (days - 1)

let startFish = sortfish input

let part1 = 
    simulateFish 6 2 80 startFish 
    |> Seq.sumBy (fun (_, c) -> c)

printfn "Part 1 Fish: %i" part1

// part 2 functions

let part2 = 
    simulateFish 6 2 256 startFish
    |> Seq.sumBy (fun (_, c) -> c)

printfn "Part 2 Fish: %i" part2