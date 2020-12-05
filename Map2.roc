interface Map2 exposes [ Dim2, size, index ] imports []


Dim2 : { rows : Int, cols : Int }


size : List Int -> Dim2
size = \cells ->
    colsHelper = \c, i ->
        when List.get c i is
            Ok 10 -> i
            _ -> colsHelper c (i + 1)
    
    cols = colsHelper cells 0

    rows =
        when List.len cells // (cols + 1) is
            Ok n -> n
            _ -> 0
    
    { rows, cols }


index : Dim2, Int, Int -> Int
index = \dim, row, col ->
    dim.cols * row + row + col
