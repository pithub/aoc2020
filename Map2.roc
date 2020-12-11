interface Map2 exposes [ Inf2, info, getI, setI ] imports []


Inf2 : { rows : I64, cols : I64, default : I64 }


info : List I64, I64 -> Inf2
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


getI : List I64, Inf2, I64, I64 -> I64
getI = \cells, inf, row, col ->
    when index inf row col is
        Ok idx ->
            when List.get cells idx is
                Ok val -> val
                _ -> inf.default
        _ ->
            inf.default


setI : List I64, Inf2, I64, I64, I64 -> List I64
setI = \cells, inf, row, col, val ->
    when index inf row col is
        Ok idx ->
            List.set cells idx val
        _ ->
            cells


index : Inf2, I64, I64 -> Result I64 {}
index = \inf, row, col ->
    if row < 0 || col < 0 || row >= inf.rows || col >= inf.cols then
        Err {}
    else
        Ok (inf.cols * row + row + col)
