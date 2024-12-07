module AoC

open System.Collections.Generic
open System.IO
open System.Runtime.CompilerServices

let private basePath = Directory.GetParent(Directory.GetCurrentDirectory()).Parent.Parent.FullName

let readInput (path: string): string array = File.ReadAllLines(Path.Combine(basePath, path))

type 'a ForgivingArray2D(rows: int, columns: int, defaultValue: 'a) =
    let array = Array.zeroCreate<'a>(rows * columns)
    member _.Rows = rows
    member _.Columns = columns
    member _.Count = rows * columns
    member _.Item
        with get(row: int, column: int) =
            if (uint row) < (uint rows) && (uint column) < (uint columns) then
                array[row * columns + column]
            else defaultValue
        and set(row: int, column: int) value =
            if (uint row) < (uint rows) && (uint column) < (uint columns) then
                array[row * columns + column] <- value
            else ()
    member _.SetRow(index: int, row: IReadOnlyList<'a>) =
        if (uint index) < (uint rows) then
            let lastIndex = (min row.Count columns) - 1

            for i in 0..lastIndex do
                array[index * columns + i] <- row[i]
        else ()

[<Extension>]
type ForgivingArray2DExtensions =
    [<Extension>]
    static member SetRow(it: char ForgivingArray2D, index: int, row: string) =
        if (uint index) < (uint it.Rows) then
            let lastIndex = (min row.Length it.Columns) - 1

            for i in 0..lastIndex do
                it[index, i] <- row[i]
        else ()