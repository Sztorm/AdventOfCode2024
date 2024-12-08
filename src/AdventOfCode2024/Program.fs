module Program

[<EntryPoint>]
let main (args: string array): int =
    let testInput = AoC.readInput "Day08/testInput.txt"
    let input = AoC.readInput "Day08/input.txt"
    printfn "part 1 test: %d" (Day08.part1 testInput)
    printfn "part 1:      %d" (Day08.part1 input)
    printfn "part 2 test: %d" (Day08.part2 testInput)
    printfn "part 2:      %d" (Day08.part2 input)
    0