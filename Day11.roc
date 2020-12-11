interface Day11 exposes [ output ] imports [ Map2, TestUtil ]


output : List I64 -> List (List I64)
output = \puzzleInput ->
    testInfo   = Map2.info testInput   0
    puzzleInfo = Map2.info puzzleInput 0

    [ TestUtil.verify 11 1 1 (occupied testInput   testInfo  ) 37
    , TestUtil.show   11 1   (occupied puzzleInput puzzleInfo)
    , TestUtil.verify 11 2 1 (occupiedV2 testInput   testInfo  ) 26
    , TestUtil.show   11 2   (occupiedV2 puzzleInput puzzleInfo)
    ]


#  first part


occupied : List I64, Map2.Inf2 -> I64
occupied = \map, inf ->
    res = step map inf
    if res.changed then
        occupied res.dstMap inf
    else
        countOccupied res.dstMap


step : List I64, Map2.Inf2 -> StepResult
step = \map, inf ->
    stepHelper map inf False map 0 0


StepResult : { changed : Bool, dstMap : List I64 }


stepHelper : List I64, Map2.Inf2, Bool, List I64, I64, I64 -> StepResult
stepHelper = \map, inf, changed, dstMap, y, x ->
    if y < inf.rows then
        if x < inf.cols then
            oldSeat = Map2.getI map inf y x

            newSeat =
                when oldSeat is
                    76 ->
                        if occupiedNeighbours map inf y x == 0 then
                            35
                        else
                            oldSeat
                    35 ->
                        if occupiedNeighbours map inf y x >= 4 then
                            76
                        else
                            oldSeat
                    _ ->
                        oldSeat

            if oldSeat == newSeat then
                stepHelper map inf changed dstMap y (x + 1)
            else
                newMap = Map2.setI dstMap inf y x newSeat
                stepHelper map inf True newMap y (x + 1)

        else
            stepHelper map inf changed dstMap (y + 1) 0
    
    else
        { changed, dstMap }


occupiedNeighbours : List I64, Map2.Inf2, I64, I64 -> I64
occupiedNeighbours = \map, inf, y, x ->
    (occupiedNum map inf (y - 1) (x - 1)) +
    (occupiedNum map inf (y - 1)  x     ) +
    (occupiedNum map inf (y - 1) (x + 1)) +
    (occupiedNum map inf  y      (x - 1)) +
    (occupiedNum map inf  y      (x + 1)) +
    (occupiedNum map inf (y + 1) (x - 1)) +
    (occupiedNum map inf (y + 1)  x     ) +
    (occupiedNum map inf (y + 1) (x + 1))


occupiedNum : List I64, Map2.Inf2, I64, I64 -> I64
occupiedNum = \map, inf, y, x ->
    if Map2.getI map inf y x == 35 then 1 else 0


countOccupied : List I64 -> I64
countOccupied = \map ->
    List.keepIf map (\c -> c == 35) |> List.len


#  second part


occupiedV2 : List I64, Map2.Inf2 -> I64
occupiedV2 = \map, inf ->
    res = stepV2 map inf
    if res.changed then
        occupiedV2 res.dstMap inf
    else
        countOccupied res.dstMap


stepV2 : List I64, Map2.Inf2 -> StepResult
stepV2 = \map, inf ->
    stepV2Helper map inf False map 0 0


stepV2Helper : List I64, Map2.Inf2, Bool, List I64, I64, I64 -> StepResult
stepV2Helper = \map, inf, changed, dstMap, y, x ->
    if y < inf.rows then
        if x < inf.cols then
            oldSeat = Map2.getI map inf y x

            newSeat =
                when oldSeat is
                    76 ->
                        if occupiedNeighboursV2 map inf y x == 0 then
                            35
                        else
                            oldSeat
                    35 ->
                        if occupiedNeighboursV2 map inf y x >= 5 then
                            76
                        else
                            oldSeat
                    _ ->
                        oldSeat

            if oldSeat == newSeat then
                stepV2Helper map inf changed dstMap y (x + 1)
            else
                newMap = Map2.setI dstMap inf y x newSeat
                stepV2Helper map inf True newMap y (x + 1)

        else
            stepV2Helper map inf changed dstMap (y + 1) 0
    
    else
        { changed, dstMap }


occupiedNeighboursV2 : List I64, Map2.Inf2, I64, I64 -> I64
occupiedNeighboursV2 = \map, inf, y, x ->
    (occupiedNumV2 map inf (y - 1) (x - 1) (-1) (-1)) +
    (occupiedNumV2 map inf (y - 1)  x      (-1) 0   ) +
    (occupiedNumV2 map inf (y - 1) (x + 1) (-1) 1   ) +
    (occupiedNumV2 map inf  y      (x - 1) 0    (-1)) +
    (occupiedNumV2 map inf  y      (x + 1) 0    1   ) +
    (occupiedNumV2 map inf (y + 1) (x - 1) 1    (-1)) +
    (occupiedNumV2 map inf (y + 1)  x      1    0   ) +
    (occupiedNumV2 map inf (y + 1) (x + 1) 1    1   )


occupiedNumV2 : List I64, Map2.Inf2, I64, I64, I64, I64 -> I64
occupiedNumV2 = \map, inf, y, x, dy, dx ->
    when Map2.getI map inf y x is
        35 -> 1
        76 -> 0
        46 -> occupiedNumV2 map inf (y + dy) (x + dx) dy dx
        _  -> 0


#  test data


testInput : List I64
testInput =
    [ 76, 46, 76, 76, 46, 76, 76, 46, 76, 76, 10
    , 76, 76, 76, 76, 76, 76, 76, 46, 76, 76, 10
    , 76, 46, 76, 46, 76, 46, 46, 76, 46, 46, 10
    , 76, 76, 76, 76, 46, 76, 76, 46, 76, 76, 10
    , 76, 46, 76, 76, 46, 76, 76, 46, 76, 76, 10
    , 76, 46, 76, 76, 76, 76, 76, 46, 76, 76, 10
    , 46, 46, 76, 46, 76, 46, 46, 46, 46, 46, 10
    , 76, 76, 76, 76, 76, 76, 76, 76, 76, 76, 10
    , 76, 46, 76, 76, 76, 76, 76, 76, 46, 76, 10
    , 76, 46, 76, 76, 76, 76, 76, 46, 76, 76, 10
    ]
