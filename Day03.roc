interface Day03 exposes [ output ] imports [ Map2, TestUtil ]


output : List I64 -> List (List I64)
output = \puzzleInput ->
    [ TestUtil.verify 3 1 1 (combinedTreeCount1 testInput  ) 7
    , TestUtil.show   3 1   (combinedTreeCount1 puzzleInput)
    , TestUtil.verify 3 2 1 (combinedTreeCount2 testInput  ) 336
    , TestUtil.show   3 2   (combinedTreeCount2 puzzleInput)
    ]


combinedTreeCount1 : List I64 -> I64
combinedTreeCount1 = \cells ->
    inf = Map2.info cells 0
    treeCount cells inf 3 1


combinedTreeCount2 : List I64 -> I64
combinedTreeCount2 = \cells ->
    inf = Map2.info cells 0
    (treeCount cells inf 1 1) *
    (treeCount cells inf 3 1) *
    (treeCount cells inf 5 1) *
    (treeCount cells inf 7 1) *
    (treeCount cells inf 1 2)


treeCount : List I64, Map2.Inf2, I64, I64 -> I64
treeCount = \cells, inf, right, down ->
    treeCountHelper cells inf right down 0 0 0


treeCountHelper : List I64, Map2.Inf2, I64, I64, I64, I64, I64 -> I64
treeCountHelper = \cells, inf, right, down, x, y, count ->
    if y < inf.rows then
        cell = Map2.getI cells inf y x

        nextCount =
            if cell == 35 then
                count + 1
            else
                count

        nextY = y + down

        incX = x + right
        nextX =
            if incX < inf.cols then
                incX
            else
                incX - inf.cols

        treeCountHelper cells inf right down nextX nextY nextCount

    else
        count


testInput : List I64
testInput =
    [ 46, 46, 35, 35, 46, 46, 46, 46, 46, 46, 46, 10
    , 35, 46, 46, 46, 35, 46, 46, 46, 35, 46, 46, 10
    , 46, 35, 46, 46, 46, 46, 35, 46, 46, 35, 46, 10
    , 46, 46, 35, 46, 35, 46, 46, 46, 35, 46, 35, 10
    , 46, 35, 46, 46, 46, 35, 35, 46, 46, 35, 46, 10
    , 46, 46, 35, 46, 35, 35, 46, 46, 46, 46, 46, 10
    , 46, 35, 46, 35, 46, 35, 46, 46, 46, 46, 35, 10
    , 46, 35, 46, 46, 46, 46, 46, 46, 46, 46, 35, 10
    , 35, 46, 35, 35, 46, 46, 46, 35, 46, 46, 46, 10
    , 35, 46, 46, 46, 35, 35, 46, 46, 46, 46, 35, 10
    , 46, 35, 46, 46, 35, 46, 46, 46, 35, 46, 35, 10
    ]
