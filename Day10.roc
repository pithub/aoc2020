interface Day10 exposes [ output ] imports [ ListExtra, ListZip, TestUtil ]


output : List I64 -> List (List I64)
output = \puzzleInput ->
    testNumbers1  = testInput1  |> TestUtil.ints |> ListExtra.quicksort
    testNumbers2  = testInput2  |> TestUtil.ints |> ListExtra.quicksort
    puzzleNumbers = puzzleInput |> TestUtil.ints |> ListExtra.quicksort

    [ TestUtil.verify 10 1 1 (differences testNumbers1 ) 35
    , TestUtil.verify 10 1 2 (differences testNumbers2 ) 220
    , TestUtil.show   10 1   (differences puzzleNumbers)
    , TestUtil.verify 10 2 1 (arrangements testNumbers1 ) 8
    , TestUtil.verify 10 2 2 (arrangements testNumbers2 ) 19208
    , TestUtil.show   10 2   (arrangements puzzleNumbers)
    ]


#  first part


differences : List I64 -> I64
differences = \input ->
    initial = { last: 0, ones: 0, threes: 1 }
    final = List.walk input diffWalker initial
    final.ones * final.threes


DiffAcc : { last : I64, ones : I64, threes : I64 }


diffWalker : I64, DiffAcc -> DiffAcc
diffWalker = \val, acc ->
    when val - acc.last is
        1 -> { acc & last: val, ones: acc.ones + 1 }
        3 -> { acc & last: val, threes: acc.threes + 1 }
        _ -> { acc & last: val }


#  second part


arrangements : List I64 -> I64
arrangements = \input ->
    initial = { last: 0, count: 0, tribs: [ 1, 1, 2 ], result: 1 }
    last = List.walk input arrWalker initial
    final = arrWalkerHelper 0 3 last
    final.result


ArrAcc : { last : I64, count : I64, tribs: List I64, result : I64 }


arrWalker : I64, ArrAcc -> ArrAcc
arrWalker = \val, acc ->
    diff = val - acc.last
    arrWalkerHelper val diff acc


arrWalkerHelper : I64, I64, ArrAcc -> ArrAcc
arrWalkerHelper = \val, diff, acc ->
    if diff == 1 then
        { acc & last: val, count: acc.count + 1 }
    else if acc.count > 1 then
        newTribs = ensureTribs acc.tribs acc.count
        factor = (ListZip.newAt newTribs acc.count 0).val
        { acc & last: val, count: 0, tribs: newTribs, result: acc.result * factor }
    else
        { acc & last: val, count: 0 }


#  Tribonacci numbers
ensureTribs : List I64, I64 -> List I64
ensureTribs = \tribs, idx ->
    len = List.len tribs
    if len > idx then
        tribs
    else
        zip0 = ListZip.newAtLast tribs 0
        zip1 = ListZip.backward zip0 tribs
        zip2 = ListZip.backward zip1 tribs
        nextTrib = zip0.val + zip1.val + zip2.val
        newTribs = List.append tribs nextTrib
        ensureTribs newTribs idx


#  test data


testInput1 : List I64
testInput1 =
    [ 49, 54, 10
    , 49, 48, 10
    , 49, 53, 10
    , 53, 10
    , 49, 10
    , 49, 49, 10
    , 55, 10
    , 49, 57, 10
    , 54, 10
    , 49, 50, 10
    , 52, 10
    ]


testInput2 : List I64
testInput2 =
    [ 50, 56, 10
    , 51, 51, 10
    , 49, 56, 10
    , 52, 50, 10
    , 51, 49, 10
    , 49, 52, 10
    , 52, 54, 10
    , 50, 48, 10
    , 52, 56, 10
    , 52, 55, 10
    , 50, 52, 10
    , 50, 51, 10
    , 52, 57, 10
    , 52, 53, 10
    , 49, 57, 10
    , 51, 56, 10
    , 51, 57, 10
    , 49, 49, 10
    , 49, 10
    , 51, 50, 10
    , 50, 53, 10
    , 51, 53, 10
    , 56, 10
    , 49, 55, 10
    , 55, 10
    , 57, 10
    , 52, 10
    , 50, 10
    , 51, 52, 10
    , 49, 48, 10
    , 51, 10
    ]
