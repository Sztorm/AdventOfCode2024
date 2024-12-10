module Program

[<EntryPoint>]
let main (args: string array): int =
    let testInput = AoC.readInput "Day10/testInput.txt"
    let input = AoC.readInput "Day10/input.txt"
    printfn "Day10:"
    printfn "part 1 test: %d" (Day10.part1 testInput)
    printfn "part 1:      %d" (Day10.part1 input)
    printfn "part 2 test: %d" (Day10.part2 testInput)
    printfn "part 2:      %d" (Day10.part2 input)
    0