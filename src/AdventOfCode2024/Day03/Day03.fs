module Day03

open System.Text.RegularExpressions

type Instruction =
    | Do
    | Dont
    | Mul of x: int64 * y: int64

let mulRegex = Regex("mul\((\d+),(\d+)\)", RegexOptions.Compiled)
let allInstructionsRegex =
    Regex("do\(\)|don't\(\)|mul\((\d+),(\d+)\)", RegexOptions.Compiled)

let part1(input: string array): int64 =
    input
    |> Array.reduce (+)
    |> mulRegex.Matches
    |> Seq.cast<Match>
    |> Seq.map (fun m ->
        let x = int64 m.Groups[1].Value
        let y = int64 m.Groups[2].Value
        
        x * y
    )
    |> Seq.sum

let part2(input: string array): int64 =
    let instructions = 
        input
        |> Array.reduce (+)
        |> allInstructionsRegex.Matches
        |> Seq.cast<Match>
        |> Seq.map(fun m ->
            match m.Value with
            | v when v.StartsWith("m") -> 
                let x = int64 m.Groups[1].Value
                let y = int64 m.Groups[2].Value

                Mul (x, y)
            | v when v.StartsWith("do(") -> Do
            | _ -> Dont
        )
        |> Seq.toArray
    
    let enabledMuls =
        let rec iter i isEnabled (products: (int64 * int64) list) =
            if i < (Array.length instructions) then
                let instruction = instructions[i]

                match (isEnabled, instruction) with
                | _, Do -> iter (i + 1) true products
                | _, Dont -> iter (i + 1) false products
                | true, Mul (x, y) -> iter (i + 1) isEnabled ((x, y)::products)
                | false, _ -> iter (i + 1) isEnabled products
            else
                products

        iter 0 true []

    enabledMuls
    |> List.sumBy (fun (x, y) -> x * y)