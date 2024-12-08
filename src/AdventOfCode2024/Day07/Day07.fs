module Day07

open System

let part1 (input: string array): int64 =
    let resultAndNumbersPairs =
        input |>
        Array.map (fun line ->
            let (resultStr, numbersStr) = line.Split(':') |> Array.toPair
            let result = int64 resultStr
            let numbers = 
                numbersStr.Split(' ', StringSplitOptions.RemoveEmptyEntries)
                |> Array.map int64
            (result, numbers)
        )
    
    let canBeEvaluated (result: int64) (numbers: int64 array) =
        let rec iter i evaluation =
            if i = (Array.length numbers) then evaluation = result
            elif evaluation > result then false
            else
                let num = numbers[i]
                let product = iter (i + 1) (evaluation * num)
                let sum = iter (i + 1) (evaluation + num)
                product || sum
        iter 1 numbers[0]

    resultAndNumbersPairs
    |> Array.sumBy (fun (result, numbers) ->
        if (canBeEvaluated result numbers) then result else 0
    )

let part2 (input: string array): int64 =
    let resultAndNumbersPairs =
        input |>
        Array.map (fun line ->
            let (resultStr, numbersStr) = line.Split(':') |> Array.toPair
            let result = int64 resultStr
            let numbers = 
                numbersStr.Split(' ', StringSplitOptions.RemoveEmptyEntries)
                |> Array.map int64
            (result, numbers)
        )
    
    let canBeEvaluated (result: int64) (numbers: int64 array) =
        let rec iter i evaluation =
            if i = (Array.length numbers) then evaluation = result
            elif evaluation > result then false
            else
                let num = numbers[i]
                let product = iter (i + 1) (evaluation * num)
                let sum = iter (i + 1) (evaluation + num)
                let concatenation = iter (i + 1) (int64 (string evaluation + string num))
                product || sum || concatenation
        iter 1 numbers[0]

    resultAndNumbersPairs
    |> Array.sumBy (fun (result, numbers) ->
        if (canBeEvaluated result numbers) then result else 0
    )