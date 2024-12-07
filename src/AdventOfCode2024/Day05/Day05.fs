module Day05

let unsortedAndSortedUpdatesPair (input: string array) =
    let pageOrderingRules =
        input
        |> Array.takeWhile (fun s -> s.Length <> 0)
        |> Array.map (fun s ->
            let xy = s.Split('|')
            let x = int xy[0]
            let y = int xy[1]

            (x, y)
        )
        |> Set

    let updates =
        input
        |> Array.skip (pageOrderingRules.Count + 1)
        |> Array.map (fun s -> s.Split(',') |> Array.map int)

    let sortedUpdates =
        updates
        |> Array.map (fun update ->
            update
            |> Array.sortWith (fun a b ->
                if (pageOrderingRules |> Set.contains (a, b)) then -1 else 1
            )
        )
        
    (updates, sortedUpdates)

let part1 (input: string array): int64 =
    input
    |> unsortedAndSortedUpdatesPair
    ||> Array.fold2 (fun acc l r ->
        acc + int64 (if (l = r) then l[Array.length l / 2] else 0)
    ) 0L

let part2 (input: string array): int64 =
    input
    |> unsortedAndSortedUpdatesPair
    ||> Array.fold2 (fun acc l r ->
        acc + int64 (if (l <> r) then r[Array.length r / 2] else 0)
    ) 0L