module Program

[<EntryPoint>]
let main (args: string array): int =
    let testInput = AoC.readInput "Day11/testInput.txt"
    let input = AoC.readInput "Day11/input.txt"
    printfn "Day11:"
    printfn "part 1 test: %d" (Day11.part1 testInput)
    printfn "part 1:      %d" (Day11.part1 input)
    printfn "part 2 test: %d" (Day11.part2 testInput)
    printfn "part 2:      %d" (Day11.part2 input)
    0