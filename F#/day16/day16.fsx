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
    |> Seq.head

// Part 1 functions

let toDecimal input = 
    Convert.ToInt32(input, 2)

let toUInt64 input = 
    Convert.ToUInt64(input, 2)

let toByte input =
    Convert.ToByte(input, 16)

let to4BitBinary (input:byte) = 
    Convert.ToString(input, 2).PadLeft(4, '0')

let toBinarySeq (input: string) =
    input
    |> Seq.map (string >> toByte >> to4BitBinary)
    |> Seq.concat
    |> Seq.map string
    |> List.ofSeq

let join (delimeter: string) (input: Collections.Generic.IEnumerable<string>) =
    String.Join(delimeter, input)

type Header = { Version: int; TypeId: int }

type Packet = 
    | Operator of Operator
    | Literal of Literal
and Operator = { Header: Header; SubPackets: List<Packet> }
and Literal =  { Header: Header; Value: uint64 }

let binaryInput = toBinarySeq input

let getHeader input = 
    let version = List.take 3 input |> join "" |> toDecimal
    let typeId = List.skip 3 input |> List.take 3 |> join "" |> toDecimal

    ({ Version = version; TypeId = typeId}, List.skip 6 input)


let rec getPacket input = 
    let (|LiteralPacket|_|) input header =
        let rec getChunk input (number: List<string>) = 
            let takeFour = List.take 4
            let skipFour = List.skip 4
            match input with
            | head::tail when head = "0" -> Some (List.append number (takeFour tail), skipFour tail)
            | head::tail when head = "1" -> getChunk (skipFour tail) (List.append number (takeFour tail))
            | _ -> None
            
        match header.TypeId with
        | 4 -> 
            //printfn "Processing Literal packet with header %A" header
            (getChunk input List.empty) 
            |> Option.map (fun (binary, remaining) -> ({ Header = header; Value = binary |> join "" |> toUInt64}, remaining))
        | _ -> None

    let (|OperatorPacket|_|) input header =
        let getPacketsByLength input = 
            let numBytes = 15
            let length = List.take numBytes input |> join "" |> toDecimal
            let rec getFromPayload input packets =
                match List.length input with
                | 0 -> packets
                | _ -> getPacket input |> fun (next, remainder) -> getFromPayload remainder (List.append packets [next])
            
            let payload =
                input 
                |> List.skip numBytes
                |> List.take length

            (getFromPayload payload List.empty, List.skip (numBytes + length) input)

        let getPacketsByNum input =
            let numBytes = 11
            let getNumberPackets = List.take numBytes input |> join "" |> toDecimal
            //printfn "Packet contains %i packets" getNumberPackets
            let rec getFromPayload input number packets =
                match number with
                | 0 -> packets, input
                | _ -> getPacket input |> fun (next, remainder) -> getFromPayload remainder (number - 1) (List.append packets [next])

            getFromPayload (List.skip 11 input) getNumberPackets List.empty

        let newOpPacket (sub, remainder) = Some ({ Header = header; SubPackets = (List.choose id sub)}, remainder)

        match header.TypeId with
        | 4 -> None
        | _ -> 
            //printfn "Processing Op Packet with header %A" header 
            match List.head input with
            | "0" -> getPacketsByLength (List.tail input) |> newOpPacket
            | "1" -> getPacketsByNum (List.tail input) |> newOpPacket
            | _ -> None

    let (header, payload) = getHeader input

    match header with
    | LiteralPacket payload (literal, remaining) -> (Some (Literal literal), remaining)
    | OperatorPacket payload (operator, remaining) -> (Some (Operator operator), remaining)
    | _ -> (None, payload)

let getVersionSum packet =
    let rec walk packets total = 
        match packets with
        | [] -> total
        | head::tail -> 
            match head with
            | Literal l -> walk tail (total + l.Header.Version)
            | Operator o -> walk (List.append o.SubPackets tail) (total + o.Header.Version)

    walk [packet] 0

let part1 = 
    binaryInput
    |> getPacket
    |> fun (p, _) -> Option.map getVersionSum p

printfn "Packet version sum: %A" part1

// Part 2 functions

let getOperation opType = 
    let boolToInt b = Some (if b then 1UL else 0UL)
    match opType with
    | 0 -> List.fold (+) 0UL >> Some
    | 1 -> List.fold (*) 1UL >> Some
    | 2 -> List.min >> Some
    | 3 -> List.max >> Some
    | 5 -> fun a -> List.head a > List.last a |> boolToInt
    | 6 -> fun a -> List.head a < List.last a |> boolToInt
    | 7 -> fun a -> List.head a = List.last a |> boolToInt
    | _ -> fun a -> None

let getResult packet =
    let rec walk packet = 
        match packet with
        | Literal l -> Some l.Value
        | Operator op -> 
            let operation = getOperation op.Header.TypeId
            op.SubPackets
            |> List.choose walk
            |> operation

    walk packet

let part2 = 
    binaryInput
    |> getPacket
    |> fun (p, _) -> Option.map getResult p |> Option.get

printfn "Packet result: %A" part2
