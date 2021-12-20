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
    //readlines "sample.txt" 
    readlines "input.txt" 

// Part 1 functions

let enhancementMap = 
    input
    |> Seq.head
    |> Seq.indexed
    |> Map.ofSeq

let originalImage = 
    input
    |> Seq.skip 2
    |> Seq.mapi (fun row line -> Seq.mapi (fun col ch -> (row, col), ch) line)
    |> Seq.concat
    |> Map.ofSeq

let getPixel image background location =
    let result = Map.tryFind location image

    match result with
    | None -> location, background
    | Some r -> location, r

let toDecimal input = 
    System.Convert.ToInt32(input, 2)

let join (delim: string) (input: seq<char>) =
    System.String.Join(delim, input)

let seqToDecimal input = 
    input
    |> Seq.map (fun c -> if c = '.' then '0' else '1')
    |> join ""
    |> toDecimal

let getRow pixel = pixel |> (fst >> fst)

let getCol pixel = pixel |> (fst >> snd)

let getMin by image =
    image |> Map.toSeq |> Seq.minBy by |> by

let getMax by image =
    image |> Map.toSeq |> Seq.maxBy by |> by

let part1Algorithm map image background (row, col) =
    seq { for r in row - 1 .. row + 1 do 
            for c in col - 1 .. col + 1 ->
                map image background (r, c)
    }
    |> Seq.map snd
    |> seqToDecimal
    |> fun i -> Map.find i enhancementMap

let enhancer algorithm image background = 
    let rowMin = (getMin getRow image) - 1
    let rowMax = (getMax getRow image) + 1
    let colMin = (getMin getCol image) - 1
    let colMax = (getMax getCol image) + 1

    seq { for r in rowMin .. rowMax do
            for c in colMin .. colMax ->
                (r, c), algorithm image background (r, c)
    }
    |> Map.ofSeq

let rec zoomEnhance enhancer times background image =
    let newBackground = 
        background
        |> Seq.replicate 9 
        |> seqToDecimal
        |> fun i -> Map.find i enhancementMap

    match times with
    | 0 -> image
    | _ -> zoomEnhance enhancer (times - 1) newBackground (enhancer image background)

let part1 =
    originalImage
    |> zoomEnhance (enhancer (part1Algorithm getPixel)) 2 '.'
    |> Map.filter (fun _ c -> c = '#')
    |> Map.count

printfn "Part 1 count: %i" part1

// part 2 functions

let part2 =
    originalImage
    |> zoomEnhance (enhancer (part1Algorithm getPixel)) 50 '.'
    |> Map.filter (fun _ c -> c = '#')
    |> Map.count

printfn "Part 2 count: %i" part2