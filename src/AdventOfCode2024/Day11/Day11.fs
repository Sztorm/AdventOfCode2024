module Day11

open System

let parseInput (input: string array) =
    input[0]
    |> _.Split(' ')
    |> Seq.map (fun s -> int64 s)

let part1 (input: string array): int64 =
    let initialStones = parseInput input
    let blink stones =
        stones
        |> Seq.collect (fun stone ->
            match stone with
            | 0L -> [|1L|]
            | _ -> 
                let stoneString = string stone
                
                if Int64.IsEvenInteger stoneString.Length then
                    let halfStringLength = stoneString.Length / 2
                    let leftHalf = int64 stoneString[..halfStringLength - 1]
                    let rightHalf = int64 stoneString[halfStringLength..]
                    [|leftHalf; rightHalf|]
                else [|stone * 2024L|]
        )
    let finalArrangement =
        let rec loop i stones =
            match i with
            | 25 -> stones
            | _ -> loop (i + 1) (blink stones)
        loop 0 initialStones

    finalArrangement
    |> Seq.length
    |> int64

let part2 (input: string array): int64 = 0