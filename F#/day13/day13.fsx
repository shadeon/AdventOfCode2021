open System.IO
open System.Text.RegularExpressions

// Get the input

let readlines (filePath: string) = seq {
    use sr = new StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine ()
}

let split (delim:string) (s:string) =
    s.Split(delim, System.StringSplitOptions.RemoveEmptyEntries)

let input = 
    //readlines "sample.txt" 
    readlines "input.txt" 

// Part 1 functions

let getPoints =
    input
    |> Seq.takeWhile (Seq.length >> (<) 0)
    |> Seq.map (split "," >> fun a -> (Seq.head a |> int, Seq.last a |> int))

let getFolds = 
    input
    |> Seq.skipWhile (fun s -> not <| s.StartsWith("fold"))

let getPaper input =
    input
    |> Seq.map (fun loc -> (loc, true))
    |> Map.ofSeq

let foldY paper y =
    let folder state key v =
        match key with
        | (_, row) when row < y -> Map.add key v state
        | (col, row) -> Map.add (col, y - (row - y) ) v state

    Map.fold folder Map.empty paper

let foldX paper x =
    let folder state key v =
        match key with
        | (col, _) when col < x -> Map.add key v state
        | (col, row) -> Map.add (x - (col - x), row) v state

    Map.fold folder Map.empty paper

let fold points instruction =
    let getfold ins =
        let m = Regex.Match(ins, @"(?<axis>x|y)=(?<loc>\d+)")
        match m.Success with
        | false -> None
        | true -> Some (m.Groups.["axis"].Value, m.Groups.["loc"].Value |> int)
    
    match getfold instruction with
    | Some ("x", loc) -> foldX points loc
    | Some ("y", loc) -> foldY points loc
    | _ -> points

let part1 = 
    let paper = getPoints |> getPaper

    getFolds
    |> Seq.take 1
    |> Seq.fold fold paper
    |> Map.count

printfn "Part 1 count: %i" part1

// part 2 functions

let part2 =
    let paper = getPoints |> getPaper

    let finalFolded = getFolds |> Seq.fold fold paper

    let colMax = 
        finalFolded
        |> Map.toSeq
        |> Seq.map (fun ((col, _), _) -> col)
        |> Seq.max

    let rowMax = 
        finalFolded
        |> Map.toSeq
        |> Seq.map (fun ((_, row), _) -> row)
        |> Seq.max
    
    let getRow row =
        seq { for i in 0 .. colMax -> if finalFolded.ContainsKey (i, row) then "#" else " "}

    seq { for i in 0 .. rowMax -> getRow i }
    |> Seq.map (fun l -> System.String.Join("", l))

part2 
    |> Seq.iter (printfn "%s")
