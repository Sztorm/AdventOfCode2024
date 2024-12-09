module Day09

open System

let part1 (input: string array): int64 =
    let compact (array: int array) =
        let rec loop i ri result =
            if i <= ri then
                match (array[i], array[ri]) with
                | (-1, -1) -> loop i (ri - 1) result
                | (-1, c) -> loop (i + 1) (ri - 1) (c::result)
                | (c, -1) -> loop (i + 1) (ri - 1) (c::result)
                | (c, _) -> loop (i + 1) ri (c::result)
            else result
        loop 0 (Array.length array - 1) []

    input[0]
    |> _.ToCharArray()
    |> Array.fold (fun (i, list) c ->
        if i % 2 = 0 then
            let file = List.init (int (Char.GetNumericValue c)) (fun _ -> i / 2)
            (i + 1, list @ file)
        else
            let space = List.init (int (Char.GetNumericValue c)) (fun _ -> -1)
            (i + 1, list @ space)
    ) (0, [])
    |> snd
    |> List.toArray
    |> compact
    |> (fun list ->
        List.foldBack (fun n (i, sum) -> (i + 1, int64 (n * i) + sum)) list (0, 0L)
    )
    |> snd

let part2 (input: string array): int64 = 0