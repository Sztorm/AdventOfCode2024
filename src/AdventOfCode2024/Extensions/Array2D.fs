module Array2D

open System.Collections.Generic

let inline findIndex ([<InlineIfLambda>] predicate: ('a -> bool)) (array: 'a array2d): int * int =
    let rec iter r c =
        if r < Array2D.length1 array then
            if c < Array2D.length2 array then
                if predicate array[r, c] then
                    (r, c)
                else
                    iter r (c + 1)
            else
                iter (r + 1) 0
        else
            raise (KeyNotFoundException())

    iter 0 0

let inline sumBy ([<InlineIfLambda>] projection: ('a -> 'b)) (array: 'a array2d): 'b =
    let rec iter r c sum =
        if r < Array2D.length1 array then
            if c < Array2D.length2 array then
                iter r (c + 1) (Checked.(+) sum (projection array[r, c]))
            else
                iter (r + 1) 0 sum
        else sum

    iter 0 0 LanguagePrimitives.GenericZero<'b>

