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

type Direction = Up | Down | Left | Right

let grid =
    input
    |> Seq.map (Seq.map (string >> int) >> List.ofSeq)
    |> List.ofSeq

let getDirection direction row column grid = 
    let coords = 
        match direction with
        | Up when row > 0 ->                                               Some (row - 1, column)
        | Left when column > 0 ->                                          Some (row, column - 1)
        | Down when row < List.length grid - 1 ->                          Some (row + 1, column)
        | Right when column < (List.item row grid |> List.length) - 1 ->   Some (row, column + 1)
        | _ -> None
    
    match coords with
    | None -> None
    | Some (x, y) -> List.item x grid |> List.item y |> Some

let isLowPoint current index rowIndex grid =
    let directions = [ getDirection Up; getDirection Down; getDirection Left; getDirection Right ]
    List.choose (fun f -> f rowIndex index grid) directions
    |> List.min
    |> (<) current // reversed due to pipeline, current is the first arg!

let foldRow grid state (index, row) =
    let folder state (col, item) =
        match isLowPoint item col index grid with
        | true -> List.append state [(item, index, col)] 
        | false -> state
        
    List.indexed row
    |> List.fold folder List.empty
    |> List.append state

let getLowPoints grid = 
    grid
    |> List.indexed
    |> List.fold (foldRow grid) List.empty
        
let part1 = 
    grid
    |> getLowPoints
    |> List.map ((fun (lp, _, _) -> lp ) >> (+) 1)
    |> List.sum

printfn "Part 1: %A" part1

// Part 2

let getAdjacent (row, col) = 
    [ 
        (row - 1, col, getDirection Up row col)
        (row + 1, col, getDirection Down row col)
        (row, col - 1, getDirection Left row col)
        (row, col + 1, getDirection Right row col)
    ]

let getBasinSize (row, col) grid = 
    let isLessThanNine value =
        match value with
        | None -> false
        | Some v -> v < 9

    let rec traverse visited next =
        let targets =
            next
            |> Seq.map getAdjacent
            |> Seq.concat
            |> Seq.distinctBy (fun (r, c, _) -> r, c)
            |> Seq.filter (fun (r, c, f) -> (not <| Set.contains (r, c) visited) && (f grid |> isLessThanNine))
            |> Seq.map (fun (r, c, _) -> (r, c))
            |> Set.ofSeq

        match Set.count targets with
        | 0 -> visited
        | _ -> traverse (Set.union visited targets) targets

    traverse Set.empty (set [ row, col ])
    |> Set.count

let part2 = 
    grid
    |> getLowPoints
    |> List.map (fun (_, r, c) -> getBasinSize (r,c) grid)
    |> List.sortDescending
    |> List.take 3
    |> List.reduce (*)

printfn "Basin sizes: %A" part2
