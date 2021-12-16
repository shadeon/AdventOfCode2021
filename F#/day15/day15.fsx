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

type Location = (int * int) * int

let length = Seq.length input - 1

let getAdjacent length (row, col) =
    let rowMin = max (row - 1) 0
    let rowMax = min (row + 1) length
    let colMin = max (col - 1) 0
    let colMax = min (col + 1) length

    [ (rowMin, col); (row, colMin); (row, colMax); (rowMax, col) ]
        |> List.where (fun (r, c) -> not <| (r = row && c = col))

let caveMap =
    let newLocation row col value = Location ((row, col), (string >> int) value)
    
    let internalMap =
        input
        |> Seq.mapi (fun row sr -> Seq.mapi (newLocation row) sr)
        |> Seq.concat
        |> Map.ofSeq
    
    fun loc -> Map.find loc internalMap

let start = (0, 0)

let manhattanDistance (destinationRow, destinationCol) (sourceRow, sourceCol)  =
    abs (destinationCol - sourceCol) + abs (destinationRow - sourceRow)

let reconstructPath cameFrom node = 
    let rec getNext camefrom node path =
        match Map.find node cameFrom with
        | None -> path
        | Some parent -> getNext camefrom parent (parent::path)

    getNext cameFrom node [node]

// Note to anyone looking at this - this is *NOT* an optimized, or even ideal implementation of A*
// in F# or any other language. openSet should be a min-heap or priority queue, not a list you
// repeatedly re-sort. Likewise fScore should be a map rather than re-computed on the fly each time.

// I did it this way because it was quicker to write & run than do it properly, and I probably won't run it again.
// In production I'd use a library and then do something else :)

let aStar (start: int * int) (goal: int * int) h caveMap getAdjacent =
    let hFunc = h (goal)

    let fScore gScore node =
        Map.tryFind node gScore
        |> Option.map ((+) (hFunc node))

    let tempgScore gScore current adj =
        gScore
        |> Map.tryFind (current)
        |> Option.map ((+) (caveMap adj))
        |> Option.map (fun score -> adj, score)

    let isImprovedScore gScore (adj, tScore) =
        gScore
        |> Map.tryFind adj
        |> Option.map ((<) tScore)
        |> Option.defaultValue true

    let rec findPath openSet cameFrom gScore = 
        match List.length openSet with
        | 0 -> None
        | _ -> 
            let current = List.head openSet
            match current with
            | g when g = goal -> Some (reconstructPath cameFrom g)
            | _ ->
                let newSet = List.tail openSet
                let improvedAdj = 
                    getAdjacent current
                    |> List.choose (tempgScore gScore current)
                    |> List.filter (isImprovedScore gScore)

                let newCameFrom =
                    improvedAdj
                    |> List.fold (fun s (n, _) -> Map.add n (Some current) s ) cameFrom
                
                let newGScore = 
                    improvedAdj
                    |> List.fold (fun s (n, score) -> Map.add n score s) gScore

                let newOpenSet = 
                    improvedAdj
                    |> List.fold (fun s (n, _) -> if List.contains n s then s else n::s ) newSet
                    |> List.sortBy (fScore newGScore)

                findPath newOpenSet newCameFrom newGScore
    
    findPath [start] (Map.ofList [ (start, None) ]) (Map.ofList [ (start, 0) ])

let getRisk caveMap start locations = 
    locations
    |> List.except [start]
    |> List.map caveMap
    |> List.sum

let part1 = 
    aStar start (length, length) manhattanDistance caveMap (getAdjacent length)
    |> Option.map (getRisk caveMap start)

printfn "Part 1 risk: %A" part1

// part 2

let pageSize = 5

let part2Length = Seq.length input * pageSize - 1

let pagedCaveMap length cavemap loc =
    let (row, col) = loc
    let original = cavemap (row % length, col % length)
    let value = original + (col / length) + (row / length)
    if value > 9 then value - 9 else value

let part2CaveMap = pagedCaveMap (Seq.length input) caveMap

let part2 = 
    aStar start (part2Length, part2Length) manhattanDistance part2CaveMap (getAdjacent part2Length)
    |> Option.map (getRisk part2CaveMap start)

printfn "Part 2 risk: %A" part2
