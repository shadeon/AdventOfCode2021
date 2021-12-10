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

let stack char existing = 
    match char with
    | '{' -> '}'::existing
    | '[' -> ']'::existing
    | '(' -> ')'::existing
    | '<' -> '>'::existing
    | '}' | ']' | ')' | '>' -> List.tail existing
    | _ -> existing

let getStacks stacks char =
    let existing = if List.length stacks > 0 then List.last stacks else []
    List.append stacks [stack char existing]

let isCorrupt stack (index, char) =
    let peekIndex = index - 1
    match peekIndex with
    | -1 -> true
    | _ ->
        let head = List.item peekIndex stack |> List.head
        match head with
        | expected when not <| (expected = char) -> true
        | _ -> false

let getCorruptedChunk stack line =
    line
    |> Seq.indexed
    |> Seq.filter (fun (_, c) -> List.contains c [ '}'; ']'; ')'; '>'] )
    |> Seq.tryFind (isCorrupt stack)
    |> Option.map (fun (_, c) -> c)

let scoreChunks input = 
    let corruptScore char =
        match char with
        | ')' -> 3
        | ']' -> 57
        | '}' -> 1197
        | '>' -> 25137
        | _ -> 0
    
    let autoCompleteScore score char = 
        score * 5.0 +
        match char with
        | ')' -> 1.0
        | ']' -> 2.0
        | '}' -> 3.0
        | '>' -> 4.0
        | _ -> 0.0
    
    let validate = 
        input
        |> Seq.map (fun l -> (l, Seq.fold getStacks [] l))
        |> Seq.map (fun (l, s) -> s, getCorruptedChunk s l)
        
    let corruptScore = 
        validate
        |> Seq.map snd
        |> Seq.filter Option.isSome
        |> Seq.map (Option.get >> corruptScore)
        |> Seq.sum

    let incomplete =
        validate
        |> Seq.filter (snd >> Option.isNone)
        |> Seq.map (fst >> Seq.last)
        |> Seq.filter (Seq.length >> (<) 0)
        |> Seq.map (Seq.fold autoCompleteScore 0.0)
        |> Seq.sort
        
    (corruptScore, incomplete |> Seq.item (Seq.length incomplete / 2))

let (part1, part2) = scoreChunks input

printfn "Part 1 score: %i" part1

printfn "Part 2 score: %A" part2