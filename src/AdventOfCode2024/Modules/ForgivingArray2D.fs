module ForgivingArray2D

open System.Collections.Generic
open System.Runtime.CompilerServices

type 'a ForgivingArray2D(rows: int, columns: int, defaultValue: 'a) =
    let array =
        let result = Array.zeroCreate<'a> (rows * columns)
        Array.fill result 0 (rows * columns) defaultValue
        result
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
    override _.ToString() =
        array
        |> Array.chunkBySize columns
        |> array2D
        |> sprintf "%A"

[<Extension>]
type ForgivingArray2DExtensions =
    [<Extension>]
    static member SetRow(it: char ForgivingArray2D, index: int, row: string) =
        if (uint index) < (uint it.Rows) then
            let lastIndex = (min row.Length it.Columns) - 1

            for i in 0..lastIndex do
                it[index, i] <- row[i]
        else ()


let inline sumBy ([<InlineIfLambda>] projection: ('a -> 'b)) (array: 'a ForgivingArray2D): 'b =
    let rec loop r c sum =
        if r < array.Rows then
            if c < array.Columns then
                loop r (c + 1) (Checked.(+) sum (projection array[r, c]))
            else
                loop (r + 1) 0 sum
        else sum

    loop 0 0 LanguagePrimitives.GenericZero<'b>