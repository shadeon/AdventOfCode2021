open System.IO

// Get the input

let readlines (filePath: string) = seq {
    use sr = new StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine ()
}

let split (delim:string) (s:string) =
    s.Split(delim, System.StringSplitOptions.RemoveEmptyEntries)

let input = 
    //readlines "sample3.txt" 
    readlines "input.txt" 

// Part 1 functions

let getExits newExit existing = 
    let target =
        match existing with
        | None -> List.empty
        | Some e -> e
    
    List.append [ newExit ] target
    |> List.distinct
    |> List.sort
    |> Some

let mapCaves input = 
    input
    |> Seq.map (split "-" >> (fun a -> [(Seq.head a, Seq.last a); (Seq.last a, Seq.head a)]))
    |> Seq.concat
    |> Seq.fold (fun m pair -> Map.change (fst pair) (getExits (snd pair)) m) Map.empty

//printfn "Cave Map: %A" <| mapCaves input

let start = "start"
let finish = "end"

let isLower (s: string) = Seq.head s |> System.Char.IsLower

let isUpper = isLower >> not

let canVisitSmallOnce visited s = 
    isUpper s || (List.contains s visited |> not)

let selectRoom predicate current visited caves = 
    let exits = 
        Map.find current caves
        |> List.where (predicate visited)

    match (List.length exits) with
    | 0 -> None // nowhere valid to go
    | _ -> Some exits

let getPaths roomSelection caves =
    let rec explore path current =
        let visited = current::path
        let exits = roomSelection current visited caves

        match (current, Option.isSome exits) with
        | (cave, _) when cave = finish -> Some [cave::visited |> List.rev]
        | (_, false) -> None
        | (_, true) -> 
            Option.get exits 
            |> List.map (explore visited)
            |> List.choose id
            |> List.concat
            |> function
                | [] -> None
                | result -> Some result

    explore list.Empty start

let part1 = 
    input
    |> mapCaves
    |> getPaths (selectRoom canVisitSmallOnce)
    |> Option.map Seq.length

printfn "Number of paths (Part 1): %A" part1

// Part 2 functions

let canVisitOneSmallTwice visited s =
    let onlyOneDuplicate = 
        List.countBy id 
        >> List.forall (fun (v, count) -> isUpper v || count < 2)

    let notContains v list = 
        List.contains v list
        |> not

    s <> start && (isUpper s || notContains s visited || onlyOneDuplicate visited)
    
let part2 =
    input
    |> mapCaves
    |> getPaths (selectRoom canVisitOneSmallTwice)
    |> Option.map Seq.length

printfn "Number of paths (Part 2): %A" part2
