module Day09

open System

let parseInput (input: string array) = input[0] |> Seq.map (fun c -> int c - int '0')

let checksum (diskLayout: int seq) =
    diskLayout
    |> Seq.fold (fun (i, sum) n ->
        match n with
        | -1 -> (i + 1, sum)
        | _ -> (i + 1, int64 (n * i) + sum)
    ) (0, 0L)
    |> snd

let part1 (input: string array): int64 =
    let decompress (diskMap: int seq) =
        diskMap
        |> Seq.fold (fun (i, list) n ->
            if Int32.IsEvenInteger i then
                let file = List.init n (fun _ -> i / 2)
                (i + 1, list @ file)
            else
                let space = List.init n (fun _ -> -1)
                (i + 1, list @ space)
        ) (0, [])
        |> snd

    let compact (diskLayout: int array) =
        let rec loop i ri result =
            if i <= ri then
                match (diskLayout[i], diskLayout[ri]) with
                | -1, -1 -> loop i (ri - 1) result
                | -1, c -> loop (i + 1) (ri - 1) (result @ [c])
                | c, -1 -> loop (i + 1) (ri - 1) (result @ [c])
                | c, _ -> loop (i + 1) ri (result @ [c])
            else result
        loop 0 (diskLayout.Length - 1) []

    input
    |> parseInput
    |> decompress
    |> List.toArray
    |> compact   
    |> checksum

let part2 (input: string array): int64 =
    let diskPairMap (diskMap: int seq) =
        diskMap
        |> Seq.fold (fun (i, list) size ->
            let id = if Int32.IsEvenInteger i then i / 2 else -1

            (i + 1, list @ [(size, id)])
        ) (0, [])
        |> snd

    let compact (diskLayout: (int * int) seq) =
        let initialDiskLayout = diskLayout |> Seq.toArray
        let rec loop i ri (result: (int * int) array) =
            if ri >= 0 then
                let right = result[ri]

                match right with
                | _, -1 -> loop 0 (ri - 1) result
                | (rs, _) ->
                    if i <= ri then
                        let left = result[i]

                        match left with
                        | ls, -1 ->
                            if ls < rs then
                                loop (i + 1) ri result
                            elif ls = rs then
                                result[i] <- right
                                result[ri] <- (rs, -1)
                                loop 0 (ri - 1) result
                            else
                                result[i] <- right
                                result[ri] <- (rs, -1)
                                loop 0 ri (result |> Array.insertAt (i + 1) (ls - rs, - 1))
                        | _ -> loop (i + 1) ri result
                    else loop 0 (ri - 1) result
            else result

        loop 0 (initialDiskLayout.Length - 1) initialDiskLayout

    let decompress (diskMap: (int * int) seq) =
        diskMap
        |> Seq.collect (fun (size, id) ->
            match (size, id) with
            | 0, _ -> Array.empty
            | _, -1 -> Array.init size (fun _ -> -1)
            | _ -> Array.init size (fun _ -> id)
        )

    input
    |> parseInput
    |> diskPairMap
    |> compact
    |> decompress
    |> checksum