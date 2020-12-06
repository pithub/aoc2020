interface Day03 exposes [ output ] imports [ Map2, TestUtil ]


output : List Int -> List (List Int)
output = \puzzleInput ->
    [ TestUtil.verify 3 1 1 (combinedTreeCount1 testInput  ) 7
    , TestUtil.show   3 1   (combinedTreeCount1 puzzleInput)
    , TestUtil.verify 3 2 1 (combinedTreeCount2 testInput  ) 336
    , TestUtil.show   3 2   (combinedTreeCount2 puzzleInput)
    ]


combinedTreeCount1 : List Int -> Int
combinedTreeCount1 = \cells ->
    inf = Map2.info cells 0
    treeCount cells inf 3 1


combinedTreeCount2 : List Int -> Int
combinedTreeCount2 = \cells ->
    inf = Map2.info cells 0
    (treeCount cells inf 1 1) *
    (treeCount cells inf 3 1) *
    (treeCount cells inf 5 1) *
    (treeCount cells inf 7 1) *
    (treeCount cells inf 1 2)


treeCount : List Int, Map2.Inf2, Int, Int -> Int
treeCount = \cells, inf, right, down ->
    treeCountHelper cells inf right down 0 0 0


treeCountHelper : List Int, Map2.Inf2, Int, Int, Int, Int, Int -> Int
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


testInput : List Int
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
