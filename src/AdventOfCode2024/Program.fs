module Program

[<EntryPoint>]
let main (args: string array): int =
    let testInput = AoC.readInput "Day06/testInput.txt"
    let input = AoC.readInput "Day06/input.txt"
    printfn "part 1 test: %d" (Day06.part1 testInput)
    printfn "part 1:      %d" (Day06.part1 input)
    printfn "part 2 test: %d" (Day06.part2 testInput)
    printfn "part 2:      %d" (Day06.part2 input)
    0
