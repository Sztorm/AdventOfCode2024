module Day04

open AoC

let part1 (input: string array): int64 =
    let wordsArray = ForgivingArray2D(Array.length input, String.length input[0], '.')
    input
    |> Array.iteri (fun i line -> wordsArray.SetRow(i, line))

    let countXmasInAllDirections r c =
        let matchMas a0 a1 a2 =
            match (a0, a1, a2) with
            | 'M', 'A', 'S' -> 1
            | _ -> 0

        if wordsArray[r, c] = 'X' then
            let wa = wordsArray
            let countRight = matchMas wa[r, c + 1] wa[r, c + 2] wa[r, c + 3]
            let countLeft = matchMas wa[r, c - 1] wa[r, c - 2] wa[r, c - 3]
            let countDown = matchMas wa[r + 1, c] wa[r + 2, c] wa[r + 3, c]
            let countUp = matchMas wa[r - 1, c] wa[r - 2, c] wa[r - 3, c]
            let countUpRight = matchMas wa[r - 1, c + 1] wa[r - 2, c + 2] wa[r - 3, c + 3]
            let countDownRight = matchMas wa[r + 1, c + 1] wa[r + 2, c + 2] wa[r + 3, c + 3]
            let countUpLeft = matchMas wa[r - 1, c - 1] wa[r - 2, c - 2] wa[r - 3, c - 3]
            let countDownLeft = matchMas wa[r + 1, c - 1] wa[r + 2, c - 2] wa[r + 3, c - 3]

            countRight + countLeft + countDown + countUp +
            countUpRight + countDownRight + countUpLeft + countDownLeft
        else 0

    let rec iter r c result =
        if r < wordsArray.Rows then
            if c < wordsArray.Columns then
                iter r (c + 1) (result + countXmasInAllDirections r c)
            else
                iter (r + 1) 0 result
        else
            result

    iter 0 0 0

let part2 (input: string array): int64 = 0