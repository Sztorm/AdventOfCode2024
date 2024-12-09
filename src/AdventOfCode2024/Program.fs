module Program

[<EntryPoint>]
let main (args: string array): int =
    let testInput = AoC.readInput "Day09/testInput.txt"
    let input = AoC.readInput "Day09/input.txt"
    printfn "Day09:"
    printfn "part 1 test: %d" (Day09.part1 testInput)
    printfn "part 1:      %d" (Day09.part1 input)
    printfn "part 2 test: %d" (Day09.part2 testInput)
    printfn "part 2:      %d" (Day09.part2 input)
    0