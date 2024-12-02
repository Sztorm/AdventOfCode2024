module Day02

let part1 (input: string array): int64 =
    let levelsArray =  
        input 
        |> Array.map (fun line -> 
            line.Split(' ') 
            |> Array.map (fun e -> int e)
        )
    let isSafe (levels: int array) =
        let rec iterator i isIncreasing levels =
            if i < (levels |> Array.length) then
                let difference = levels[i] - levels[i - 1]

                match (isIncreasing, difference) with
                | true, d when d >= 1 && d <= 3 ->
                    iterator (i + 1) isIncreasing levels
                | false, d when d <= -1 && d >= -3 ->
                    iterator (i + 1) isIncreasing levels
                | _ -> false
            else 
                true

        iterator 1 (levels[1] > levels[0]) levels

    levelsArray
    |> Array.sumBy (fun levels -> if isSafe levels then 1 else 0)

let part2 (input: string array): int64 =
    let levelsArray =  
        input 
        |> Array.map (fun line -> 
            line.Split(' ') 
            |> Array.map (fun e -> int e)
        )
    let isSafe (levels: int array) =
        let rec iterator i isIncreasing levels =
            if i < (levels |> Array.length) then
                let difference = levels[i] - levels[i - 1]

                match (isIncreasing, difference) with
                | true, d when d >= 1 && d <= 3 ->
                    iterator (i + 1) isIncreasing levels
                | false, d when d <= -1 && d >= -3 ->
                    iterator (i + 1) isIncreasing levels
                | _ -> false
            else 
                true

        let levelsCombinations =
            Array.init (levels |> Array.length) (fun i -> levels |> (Array.removeAt i))
            |> Array.append [| levels |]

        levelsCombinations
        |> Array.exists (fun combination -> 
            iterator 1 (combination[1] > combination[0]) combination
        )

    levelsArray
    |> Array.sumBy (fun levels -> if isSafe levels then 1 else 0)