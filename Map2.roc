interface Map2 exposes [ Inf2, info, getI ] imports []


Inf2 : { rows : Int, cols : Int, default : Int }


info : List Int, Int -> Inf2
info = \cells, default ->
    colsHelper = \c, i ->
        when List.get c i is
            Ok 10 -> i
            _ -> colsHelper c (i + 1)
    
    cols = colsHelper cells 0

    rows =
        when List.len cells // (cols + 1) is
            Ok n -> n
            _ -> 0
    
    { rows, cols, default }


getI : List Int, Inf2, Int, Int -> Int
getI = \cells, inf, row, col ->
    when index inf row col is
        Ok idx ->
            when List.get cells idx is
                Ok val -> val
                _ -> inf.default
        _ -> inf.default


index : Inf2, Int, Int -> Result Int {}
index = \inf, row, col ->
    if row < 0 || col < 0 || row >= inf.rows || col >= inf.cols then
        Err {}
    else
        Ok (inf.cols * row + row + col)
