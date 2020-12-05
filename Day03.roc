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
    dim = Map2.size cells
    treeCount cells dim 3 1


combinedTreeCount2 : List Int -> Int
combinedTreeCount2 = \cells ->
    dim = Map2.size cells
    (treeCount cells dim 1 1) *
    (treeCount cells dim 3 1) *
    (treeCount cells dim 5 1) *
    (treeCount cells dim 7 1) *
    (treeCount cells dim 1 2)


treeCount : List Int, Map2.Dim2, Int, Int -> Int
treeCount = \cells, dim, right, down ->
    treeCountHelper cells dim right down 0 0 0


treeCountHelper : List Int, Map2.Dim2, Int, Int, Int, Int, Int -> Int
treeCountHelper = \cells, dim, right, down, x, y, count ->
    idx = Map2.index dim y x
    when List.get cells idx is
        Ok cell ->
            nextCount =
                if cell == 35 then
                    count + 1
                else
                    count

            nextY = y + down

            incX = x + right
            nextX =
                if incX < dim.cols then
                    incX
                else
                    incX - dim.cols

            treeCountHelper cells dim right down nextX nextY nextCount

        _ -> count


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
