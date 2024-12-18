module Program

[<EntryPoint>]
let main (args: string array): int =
    let testInput = AoC.readInput "Day12/testInput.txt"
    let input = AoC.readInput "Day12/input.txt"
    printfn "Day12:"
    printfn "part 1 test: %d" (Day12.part1 testInput)
    printfn "part 1:      %d" (Day12.part1 input)
    printfn "part 2 test: %d" (Day12.part2 testInput)
    printfn "part 2:      %d" (Day12.part2 input)
    0