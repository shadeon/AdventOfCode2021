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

let split (delim:string, s:string) =
    s.Split(delim, StringSplitOptions.RemoveEmptyEntries)

let splitSpaces s = split(" ", s)

let numbers = 
    split (",", (Seq.head input))
    |> Seq.map int
    
let newRow index line = 
    splitSpaces line
    |> Seq.map int
    |> Seq.mapi (fun col num -> num, index, col)

let newBoard lines = 
    Seq.mapi newRow lines
    |> Seq.concat

let rec getBoards lines boards =
    let board = seq { newBoard (Seq.take 5 lines) }
    let rest = Seq.skip 5 lines
    
    match Seq.isEmpty rest with
    | true -> Seq.append boards board
    | false -> getBoards (Seq.tail rest) (Seq.append boards board)

let boards = getBoards (Seq.skip 2 input) Seq.empty

let row (_, row, _) = row

let column (_, _, col) = col

let number (num, _, _) = num

let isMatched drawn (number, _, _) = 
    Seq.contains number drawn

let getMatched drawn =
    Seq.where (isMatched drawn)

let getUnMatched drawn =
    Seq.where (isMatched drawn >> not)

let hasWon drawn board = 
    let matched = getMatched drawn board
    let isLine (_, c) = c > 4
    let rows = 
        Seq.countBy row matched
        |> Seq.exists isLine
    
    let cols = 
        Seq.countBy column matched
        |> Seq.exists isLine
    
    match (rows, cols) with
    | (true, _) -> true
    | (_, true) -> true
    | _ -> false

let calculateScore drawn board =
    let unmatched = getUnMatched drawn board
    Seq.last drawn * Seq.sumBy number unmatched

let rec findScore numbers index boards = 
    let drawn = Seq.take index numbers
    let winner = Seq.tryFind (hasWon drawn) boards

    match winner with
    | Some board -> calculateScore drawn board
    | _ -> findScore numbers (index + 1) boards

let getFirstScore = findScore

let part1 = getFirstScore numbers 5 boards 

printfn "Part 1 winning board score: %i" part1

// part 2 functions

let rec findLastScore numbers index last boards = 
    let drawn = Seq.take index numbers
    let loser = Seq.tryFind (hasWon drawn >> not) boards

    match loser with
    | Some loser -> calculateScore (seq {yield! drawn; yield last}) loser
    | _ -> findLastScore numbers (index - 1) (Seq.last drawn) boards

let part2 = findLastScore numbers ((Seq.length numbers) - 1) (Seq.last numbers) boards

printfn "Part 2 winning board score: %i" part2
