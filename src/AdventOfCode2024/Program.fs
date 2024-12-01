module Program

[<EntryPoint>]
let main (args: string array): int =
    let input = AoC.readInput "Day01/input.txt"
    printfn "%d" (Day01.part1 input)
    printfn "%d" (Day01.part2 input)
    0
