module Day08

open ForgivingArray2D

let part1 (input: string array): int64 =
    let positionsByAntenna =
        let rec loop r c result =
            if r < Array.length input then
                if c < String.length input[r] then
                    match input[r][c] with
                    | '.' -> loop r (c + 1) result
                    | antenna ->
                        match result |> Map.tryFind antenna with
                        | Some positions ->
                            loop r (c + 1) (result |> Map.add antenna ((r, c)::positions))
                        | None -> loop r (c + 1) (result |> Map.add antenna [(r, c)])
                else
                    loop (r + 1) 0 result
            else
                result
        loop 0 0 Map.empty

    let antinodeMap = ForgivingArray2D(Array.length input, String.length input[0], '.')

    positionsByAntenna
    |> Map.values
    |> Seq.iter (fun positions ->
        let rec loop remainingPositions =
            match remainingPositions with
            | [] -> ()
            | (r1, c1)::tail ->
                tail
                |> List.iter (fun (r2, c2) -> (
                    antinodeMap[2 * r1 - r2, 2 * c1 - c2] <- '#'
                    antinodeMap[2 * r2 - r1, 2 * c2 - c1] <- '#'
                ))
                loop tail
        loop positions
    )

    antinodeMap
    |> ForgivingArray2D.sumBy (fun p -> if p = '#' then 1 else 0)

let part2 (input: string array): int64 = 0