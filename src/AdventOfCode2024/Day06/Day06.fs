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
                    | Up -> iter (r + 1) (c + 1) Left
                    | Left -> iter (r + 1) (c - 1) Down
                    | Down -> iter (r - 1) (c - 1) Right
                    | Right -> iter (r - 1) (c + 1) Up
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

let part2 (input: string array): int64 =
    let map = input |> array2D
    let guardPosition = map |> Array2D.findIndex (fun e -> e = '^')
    let bufferedMap = map |> Array2D.copy
    let countLoopingWithObstacleAt obR obC =
        Array2D.blit map 0 0 bufferedMap 0 0 (Array2D.length1 map) (Array2D.length2 map)
        let (gr, gc) = guardPosition
        bufferedMap[obR, obC] <- '#'

        let rec iter r c direction stepSet =
            match (r, c) with
            | r, c when (
                r >= 0 && r < Array2D.length1 map &&
                c >= 0 && c < Array2D.length2 map
                ) ->
                let step = (r, c, direction)

                if stepSet |> Set.contains step then
                    1L
                else
                    let updatedStepSet = stepSet |> Set.add step

                    match bufferedMap[r, c] with
                    | '#' ->
                        match direction with
                        | Up -> iter (r + 1) (c + 1) Left updatedStepSet
                        | Left -> iter (r + 1) (c - 1) Down updatedStepSet
                        | Down -> iter (r - 1) (c - 1) Right updatedStepSet
                        | Right -> iter (r - 1) (c + 1) Up updatedStepSet
                    | _ ->
                        match direction with
                        | Up -> iter (r - 1) c direction updatedStepSet
                        | Left -> iter r (c + 1) direction updatedStepSet
                        | Down -> iter (r + 1) c direction updatedStepSet
                        | Right -> iter r (c - 1) direction updatedStepSet
            | _ -> 0L
        
        iter gr gc Up Set.empty
    
    let loopingPositionsSum =
        let rec iter r c sum =
            if r < Array2D.length1 map then
                if c < Array2D.length2 map then
                    match map[r, c] with
                    | '^' | '#' -> iter r (c + 1) sum
                    | _ ->
                        let loopingResult = countLoopingWithObstacleAt r c
                        iter r (c + 1) (sum + loopingResult)              
                else
                    iter (r + 1) 0 sum
            else sum
        iter 0 0 0L

    loopingPositionsSum