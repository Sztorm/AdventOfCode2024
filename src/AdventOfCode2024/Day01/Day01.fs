module Day01

open System

let part1 (input: string array): int64 =
    input 
    |> Array.map (fun s -> 
        let stringPair = s.Split(' ', StringSplitOptions.RemoveEmptyEntries)
        (int64 stringPair[0], int64 stringPair[1])
    )
    |> Array.unzip
    |> (fun (leftArray, rightArray) ->
        let sortedLA = leftArray |> Array.sort
        let sortedRA = rightArray |> Array.sort
        Array.map2 (fun l r -> abs (l - r)) sortedLA sortedRA
       )
    |> Array.sum

let part2 (input: string array): int64 =
    let inputPairs =
        input 
        |> Array.map (fun s -> 
            let stringPair = s.Split(' ', StringSplitOptions.RemoveEmptyEntries)
            (int64 stringPair[0], int64 stringPair[1])
        )
    let occurenceCountsByNumber = 
        inputPairs
        |> Array.countBy (fun (_, right) ->  right)
        |> Map
    inputPairs
    |> Array.sumBy (fun (left, _) ->
        match occurenceCountsByNumber |> Map.tryFind left with
        | Some count -> int64 count * left
        | None -> 0
    )