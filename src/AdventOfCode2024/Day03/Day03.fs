module Day03

open System.Text.RegularExpressions

let private regex = Regex("mul\((\d+),(\d+)\)", RegexOptions.Compiled)

let part1 (input: string array): int64 =
    input
    |> Array.reduce (fun acc line -> acc + line)
    |> regex.Matches
    |> Seq.cast<Match>
    |> Seq.map (fun m ->
        let x = int64 m.Groups[1].Value
        let y = int64 m.Groups[2].Value
        
        x * y
    )
    |> Seq.sum

let part2 (input: string array): int64 = 0