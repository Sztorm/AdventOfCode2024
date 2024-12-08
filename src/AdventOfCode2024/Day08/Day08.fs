module Day08

open ForgivingArray2D

let positionsByAntenna (input: string array): Map<char, (int * int) list> =
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

let part1 (input: string array): int64 =
    let positionsByAntenna = positionsByAntenna input
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
                    let (dr1, dc1) = (r1 - r2, c1 - c2)
                    let (dr2, dc2) = (r2 - r1, c2 - c1)

                    antinodeMap[r1 + dr1, c1 + dc1] <- '#'
                    antinodeMap[r2 + dr2, c2 + dc2] <- '#'
                ))
                loop tail
        loop positions
    )

    antinodeMap
    |> ForgivingArray2D.sumBy (fun p -> if p = '#' then 1 else 0)

let part2 (input: string array): int64 =
    let positionsByAntenna = positionsByAntenna input
    let antinodeMap = ForgivingArray2D(Array.length input, String.length input[0], '.')

    positionsByAntenna
    |> Map.values
    |> Seq.iter (fun positions ->
        let rec loop remainingPositions =
            match remainingPositions with
            | [] -> ()
            | (r1, c1)::tail ->
                antinodeMap[r1, c1] <- '#'
                tail
                |> List.iter (fun (r2, c2) -> (
                    let (dr1, dc1) = (r1 - r2, c1 - c2)
                    let (dr2, dc2) = (r2 - r1, c2 - c1)

                    let rec loop2 r c rStep cStep =
                        let nextR = (r + rStep)
                        let nextC = (c + cStep)
                        if nextR >= 0 && nextR < antinodeMap.Rows &&
                            nextC >= 0 && nextC < antinodeMap.Columns then
                            antinodeMap[nextR, nextC] <- '#'
                            loop2 nextR nextC rStep cStep
                        else ()

                    loop2 r1 c1 dr1 dc1
                    loop2 r2 c2 dr2 dc2
                ))
                loop tail
        loop positions
    )

    antinodeMap
    |> ForgivingArray2D.sumBy (fun p -> if p = '#' then 1 else 0)