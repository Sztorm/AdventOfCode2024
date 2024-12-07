module Day06

type Direction =
    | Up
    | Left
    | Down
    | Right

let part1 (input: string array): int64 =
    let map = input |> array2D
    let guardPosition = map |> Array2D.findIndex (fun e -> e = '^')
    let markVisitedPositions =
        let rec iter r c direction =
            match (r, c) with
            | r, c when (
                r >= 0 && r < Array2D.length1 map &&
                c >= 0 && c < Array2D.length2 map
                ) ->
                match map[r, c] with
                | '#' ->
                    match direction with
                    | Up -> iter (r + 1) c Left
                    | Left -> iter r (c - 1) Down
                    | Down -> iter (r - 1) c Right
                    | Right -> iter r (c + 1) Up
                | _ ->
                    map[r, c] <- 'X'
                    match direction with
                    | Up -> iter (r - 1) c direction
                    | Left -> iter r (c + 1) direction
                    | Down -> iter (r + 1) c direction
                    | Right -> iter r (c - 1) direction
            | _ -> () 
        
        let (r, c) = guardPosition
        iter r c Up

    markVisitedPositions
    map
    |> Array2D.sumBy (fun e -> if e = 'X' then 1L else 0L)

let part2 (input: string array): int64 = 0