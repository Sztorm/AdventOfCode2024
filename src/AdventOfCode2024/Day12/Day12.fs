module Day12

open ForgivingArray2D

let part1 (input: string array): int64 =
    let map = ForgivingArray2D.init input.Length input[0].Length '.' (fun r c ->
        input[r][c]
    )
    let uniqueRegionMap =
        let result = ForgivingArray2D(map.Rows, map.Columns, '.')
        let rec traverse r c plant newPlant =
            match map[r, c] with
            | p when p = plant ->
                result[r, c] <- newPlant
                map[r, c] <- '.'
                traverse r (c + 1) plant newPlant
                traverse r (c - 1) plant newPlant
                traverse (r + 1) c plant newPlant
                traverse (r - 1) c plant newPlant
            | _ -> ()     
        let rec loop newPlant =
            match map |> ForgivingArray2D.tryFindIndex (fun p -> p <> '.') with
            | Some (r, c) ->
                traverse r c map[r, c] newPlant
                loop (char (int newPlant + 1))
            | None -> ()
        loop '1'
        result
    let areasByPlant =
        let rec loop r c result =
            if r < uniqueRegionMap.Rows then
                if c < uniqueRegionMap.Columns then
                    let plant = uniqueRegionMap[r, c]
                    match result |> Map.tryFind plant with
                    | Some count -> loop r (c + 1) (result |> Map.add plant (count + 1L))
                    | None -> loop r (c + 1) (result |> Map.add plant 1L)
                else
                    loop (r + 1) 0 result
            else
                result
        loop 0 0 Map.empty
    let perimetersByPlant =      
        let horizontalSides perimetersMap =
            let rec loop r c result prevPlant =
                if r < uniqueRegionMap.Rows then
                    if c < uniqueRegionMap.Columns then
                        let plant = uniqueRegionMap[r, c]

                        if prevPlant <> plant then
                            match result |> Map.tryFind plant with
                            | Some count ->
                                loop r (c + 1) (result |> Map.add plant (count + 2L)) plant
                            | None ->
                                loop r (c + 1) (result |> Map.add plant 2L) plant
                        else
                            loop r (c + 1) result plant
                    else
                        loop (r + 1) 0 result prevPlant
                else
                    result
            loop 0 0 perimetersMap '.'
        let verticalSides perimetersMap =
            let rec loop r c result prevPlant =
                if c < uniqueRegionMap.Columns then
                    if r < uniqueRegionMap.Rows then      
                        let plant = uniqueRegionMap[r, c]

                        if prevPlant <> plant then
                            match result |> Map.tryFind plant with
                            | Some count ->
                                loop (r + 1) c (result |> Map.add plant (count + 2L)) plant
                            | None ->
                                loop (r + 1) c (result |> Map.add plant 2L) plant
                        else
                            loop (r + 1) c result plant
                    else
                        loop 0 (c + 1) result prevPlant
                else
                    result
            loop 0 0 perimetersMap '.'
        Map.empty
        |> horizontalSides
        |> verticalSides
    
    areasByPlant
    |> Map.keys
    |> Seq.sumBy (fun plant -> areasByPlant[plant] * perimetersByPlant[plant])

let part2 (input: string array): int64 = 0