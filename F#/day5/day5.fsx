open System.Text.RegularExpressions
open System.IO
open System

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

type point = int * int

type line = point * point

let (|Line|_|) input = 
    let m = Regex.Match(input, @"(?<x1>\d+),(?<y1>\d+) -> (?<x2>\d+),(?<y2>\d+)")
    let value (loc: string) = int m.Groups.[loc].Value
    match m.Success with
    | true -> Some (line(point(value "x1",value "y1") , point(value "x2",value "y2")))
    | false -> None

let getLine input = 
    match input with
    | Line l -> Some l
    | _ -> None        

let lines = 
    Seq.map getLine input
    |> Seq.where Option.isSome
    |> Seq.map Option.get

let getPoints (start, finish) = 
    let (startX, startY) = start
    let (finishX, finishY) = finish
    
    let by val1 val2 = 
        if val2 < val1 then -1 else 1

    let padWithLast source length =
        match Seq.length source with
        | l when l < length -> Seq.append source <| Seq.replicate (length - l) (Seq.last source)
        | _ -> source

    let step start finish = seq {for i in start .. by start finish .. finish -> i}

    let stepX = step startX finishX
    let stepY = step startY finishY

    Seq.map2 (fun x y -> x, y) (padWithLast stepX (Seq.length stepY)) (padWithLast stepY (Seq.length stepX))

let isStraight line = 
    match line with
    | ((x1, _), (x2, _)) when x1 = x2 -> true
    | ((_, y1), (_, y2)) when y1 = y2 -> true
    | _ -> false

let getCounts predicate input =
    input
    |> Seq.where predicate
    |> Seq.map getPoints
    |> Seq.concat
    |> Seq.countBy (fun p -> p)

let countIntersections input =
    input
    |> Seq.where (fun (_, c) -> c > 1 )
    |> Seq.length

let part1 = getCounts isStraight lines |> countIntersections

printfn "Part 1 answer: %i" part1

// part 2 functions

let part2 = getCounts (fun _ -> true) lines |> countIntersections

printfn "Part 2 answer: %i" part2
