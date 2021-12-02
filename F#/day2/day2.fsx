open System.Text.RegularExpressions
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

type direction = Forward | Up | Down

let (|Instruction|_|) input =
    let m = Regex.Match(input, @"(?<direction>\D+)\s(?<distance>\d+)")
    if m.Success = true then
        let amount = int m.Groups.["distance"].Value
        match m.Groups.["direction"].Value with
        | "forward" -> Some (Forward, amount)
        | "down" -> Some (Down, amount)
        | "up" -> Some (Up, amount)
        | _ -> None
    else None

let simpleDirection instruction (hori, depth) =
    match instruction with
    | (Forward, f) -> hori + f, depth
    | (Down, d) -> hori, depth + d
    | (Up, u) -> hori, depth - u

let moveSub propulsion position instruction =
    match instruction with
    | Instruction direction-> propulsion direction position
    | _ -> position

let multiplyCoOrds (horizontal, depth) = horizontal * depth

let part1 = 
    Seq.fold (moveSub simpleDirection) (0,0) input
    |> multiplyCoOrds

printfn "Part 1 position: %i" part1

// Part 2 functions

let aimDirection instruction (hori, depth, aim) =
    match instruction with
    | (Down, d) -> hori, depth, aim + d
    | (Up, u) -> hori, depth, aim - u
    | (Forward, f) -> hori + f, depth + aim * f , aim

let getCoOrds (hori, depth, _) = (hori, depth)

let part2 = 
    Seq.fold (moveSub aimDirection) (0,0,0) input
    |> (fun (h, v, _) -> h, v) // How is there not an implicit way to go from a triple to a pair???
    |> multiplyCoOrds

printfn "Part 2 position: %i" part2
