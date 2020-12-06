interface Day01 exposes [ output ] imports [ ListExtra, ListZip, TestUtil ]


output : List Int -> List (List Int)
output = \puzzleInput ->
    sortedTestInput   = testInput   |> TestUtil.ints |> ListExtra.quicksort
    sortedPuzzleInput = puzzleInput |> TestUtil.ints |> ListExtra.quicksort

    [ TestUtil.verify 1 1 1 (solution2 sortedTestInput  ) 514579
    , TestUtil.show   1 1   (solution2 sortedPuzzleInput)
    , TestUtil.verify 1 2 1 (solution3 sortedTestInput  ) 241861950
    , TestUtil.show   1 2   (solution3 sortedPuzzleInput)
    ]


solution2 : List Int -> Int
solution2 = \list ->
    lo = ListZip.new list 0
    hi = ListZip.last lo list

    search2Values list lo hi


search2Values : List Int, ListZip.Zip, ListZip.Zip -> Int
search2Values = \list, lo, hi ->
    sum = lo.val + hi.val

    if sum == 2020 then
        lo.val * hi.val

    else if sum < 2020 then
        nextLo = ListZip.forward lo list
        search2Values list nextLo hi

    else
        nextHi = ListZip.backward hi list
        search2Values list lo nextHi


solution3 : List Int -> Int
solution3 = \list ->
    lo = ListZip.new list 0
    mid = ListZip.forward lo list
    hi = ListZip.last lo list

    search3Values list lo mid hi


search3Values : List Int, ListZip.Zip, ListZip.Zip, ListZip.Zip -> Int
search3Values = \list, lo, mid, hi ->
    sum = lo.val + mid.val + hi.val

    if sum == 2020 then
        lo.val * mid.val * hi.val

    else if sum < 2020 then
        if mid.idx + 1 < hi.idx then
            nextMid = ListZip.forward mid list
            search3Values list lo nextMid hi
        else
            nextLo = ListZip.forward lo list
            nextMid = ListZip.forward lo list
            nextHi = ListZip.last lo list
            search3Values list nextLo nextMid nextHi

    else
        if mid.idx < hi.idx - 1 then
            nextHi = ListZip.backward hi list
            search3Values list lo mid nextHi
        else
            nextLo = ListZip.forward lo list
            nextMid = ListZip.forward lo list
            nextHi = ListZip.last lo list
            search3Values list nextLo nextMid nextHi


testInput : List Int
testInput =
    [ 49, 55, 50, 49, 10
    , 57, 55, 57, 10
    , 51, 54, 54, 10
    , 50, 57, 57, 10
    , 54, 55, 53, 10
    , 49, 52, 53, 54, 10
    ]
