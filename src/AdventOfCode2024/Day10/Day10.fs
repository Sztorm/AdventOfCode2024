module Day10

open ForgivingArray2D

type Direction = Up | Left | Down | Right

let findTrailheadPositions (map: ForgivingArray2D<int>) =
    let rec loop r c positions =
        if r < map.Rows then
            if c < map.Columns then 
                match map[r, c] with
                | 0 -> loop r (c + 1) (positions @ [(r, c)])
                | _ -> loop r (c + 1) positions
            else
                loop (r + 1) 0 positions
        else positions
    loop 0 0 []

let part1 (input: string array): int64 =
    let map = ForgivingArray2D.init input.Length input[0].Length -1 (fun r c ->
        int (input[r][c]) - int '0'
    )
    let trailheadPositions = findTrailheadPositions map

    let trailheadScore position =
        let rec traverse r c direction peaks =
            let current = map[r, c]
            match direction with
            | Up ->
                match map[r - 1, c] with
                | p when p = current + 1 && p = 9 ->
                    peaks |> Set.add (r - 1, c)
                | p when p = current + 1 ->
                    peaks
                    |> Set.union (traverse (r - 1) c Up peaks)
                    |> Set.union (traverse (r - 1) c Left peaks)
                    |> Set.union (traverse (r - 1) c Right peaks)
                | _ -> peaks
            | Left ->
                match map[r, c - 1] with
                | p when p = current + 1 && p = 9 ->
                    peaks |> Set.add (r, c - 1)
                | p when p = current + 1 ->
                    peaks
                    |> Set.union (traverse r (c - 1) Up peaks)
                    |> Set.union (traverse r (c - 1) Left peaks)
                    |> Set.union (traverse r (c - 1) Down peaks)
                | _ -> peaks
            | Down ->
                match map[r + 1, c] with
                | p when p = current + 1 && p = 9 ->
                    peaks |> Set.add (r + 1, c)
                | p when p = current + 1 ->
                    peaks
                    |> Set.union (traverse (r + 1) c Left peaks)
                    |> Set.union (traverse (r + 1) c Down peaks)
                    |> Set.union (traverse (r + 1) c Right peaks)
                | _ -> peaks
                
            | Right ->
                match map[r, c + 1] with
                | p when p = current + 1 && p = 9 ->
                    peaks |> Set.add (r, c + 1)
                | p when p = current + 1 ->
                    peaks
                    |> Set.union (traverse r (c + 1) Up peaks)
                    |> Set.union (traverse r (c + 1) Down peaks)
                    |> Set.union (traverse r (c + 1) Right peaks)
                | _ -> peaks
        let (thR, thC) = position
        Set.empty
        |> Set.union (traverse thR thC Up Set.empty)
        |> Set.union (traverse thR thC Left Set.empty)
        |> Set.union (traverse thR thC Down Set.empty)
        |> Set.union (traverse thR thC Right Set.empty)
        |> Set.count

    trailheadPositions
    |> List.sumBy (fun position -> trailheadScore position)

let part2 (input: string array): int64 =
    let map = ForgivingArray2D.init input.Length input[0].Length -1 (fun r c ->
        int (input[r][c]) - int '0'
    )
    let trailheadPositions = findTrailheadPositions map
    let trailheadRating position =
        let (thR, thC) = position
        let rec traverse r c direction (result: int) =
            let current = map[r, c]
            match direction with
            | Up ->
                match map[r - 1, c] with
                | p when p = current + 1 && p = 9 ->
                    result + 1
                | p when p = current + 1 ->               
                    (traverse (r - 1) c Up result) +
                    (traverse (r - 1) c Left result) +
                    (traverse (r - 1) c Right result)
                | _ -> result
            | Left ->
                match map[r, c - 1] with
                | p when p = current + 1 && p = 9 ->
                    result + 1
                | p when p = current + 1 ->       
                    (traverse r (c - 1) Up result) +
                    (traverse r (c - 1) Left result) +
                    (traverse r (c - 1) Down result)
                | _ -> result  
            | Down ->
                match map[r + 1, c] with
                | p when p = current + 1 && p = 9 ->
                    result + 1
                | p when p = current + 1 -> 
                    (traverse (r + 1) c Left result) +
                    (traverse (r + 1) c Down result) +
                    (traverse (r + 1) c Right result)
                | _ -> result
                
            | Right ->
                match map[r, c + 1] with
                | p when p = current + 1 && p = 9 ->
                    result + 1
                | p when p = current + 1 -> 
                    (traverse r (c + 1) Up result) +
                    (traverse r (c + 1) Down result) +
                    (traverse r (c + 1) Right result)
                | _ -> result
        (traverse thR thC Up 0) +
        (traverse thR thC Left 0) +
        (traverse thR thC Down 0) +
        (traverse thR thC Right 0)
    
    trailheadPositions
    |> List.sumBy (fun position -> trailheadRating position)