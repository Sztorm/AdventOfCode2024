module Day11

open System

let parseInput (input: string array) =
    input[0]
    |> _.Split(' ')
    |> Seq.map (fun s -> int64 s)
    |> Seq.countBy id
    |> Seq.map (fun (s, c) -> (s, int64 c))
    |> Map

let blink stone count =
    match stone with
    | 0L -> Map [(1L, count)]
    | _ -> 
        let stoneString = string stone
        
        if Int64.IsEvenInteger stoneString.Length then
            let halfStringLength = stoneString.Length / 2
            let leftHalf = int64 stoneString[..halfStringLength - 1]
            let rightHalf = int64 stoneString[halfStringLength..]

            if (rightHalf = leftHalf) then
                Map [(leftHalf, 2L * count)]
            else
                Map [(leftHalf, count); (rightHalf, count)]
        else
            Map [(stone * 2024L, count)]


let arrangementLength blinkCount initialStones =
    let rec loop i stones =
        match i with
        | i when i = blinkCount -> stones
        | _ ->
            let newStones =
                stones
                |> Map.fold (fun acc stone count ->
                    let blinkStones = 
                        blink stone count
                        |> Map.map (fun s c ->
                            match acc |> Map.tryFind s with
                            | Some prevCount -> prevCount + c
                            | None -> c
                        )
                    acc
                    |> (fun a -> Map.foldBack Map.add blinkStones a)
                ) Map.empty
            loop (i + 1) newStones

    loop 0 initialStones
    |> Map.fold (fun acc _ count -> acc + count) 0L

let part1 (input: string array): int64 =
    input
    |> parseInput 
    |> arrangementLength 25

let part2 (input: string array): int64 =
    input
    |> parseInput 
    |> arrangementLength 75