interface Day09 exposes [ output ] imports [ ListExtra, ListZip, TestUtil ]


output : List I64 -> List (List I64)
output = \puzzleInput ->
    testInts   = TestUtil.ints testInput
    puzzleInts = TestUtil.ints puzzleInput

    testInvalid   = firstInvalid  5 testInts
    puzzleInvalid = firstInvalid 25 puzzleInts

    [ TestUtil.verify 9 1 1 testInvalid   127
    , TestUtil.show   9 1   puzzleInvalid
    , TestUtil.verify 9 2 1 (weakness testInvalid   testInts  ) 62
    , TestUtil.show   9 2   (weakness puzzleInvalid puzzleInts)
    ]


#  first part


firstInvalid : I64, List I64 -> I64
firstInvalid = \len, list ->
    start = ListZip.newAtFirst list 0
    next = ListZip.moveTo start list len
    firstInvalidHelper len list start next


firstInvalidHelper : I64, List I64, ListZip.Zip, ListZip.Zip -> I64
firstInvalidHelper = \len, list, start, next ->
    sorted = sortedSubList list start len
    subStart = ListZip.newAtFirst sorted 0
    subEnd = ListZip.last subStart sorted

    if pairExists sorted next.val subStart subEnd then
        newStart = ListZip.forward start list
        newNext = ListZip.forward next list
        firstInvalidHelper len list newStart newNext
    else
        next.val


pairExists : List I64, I64, ListZip.Zip, ListZip.Zip -> Bool
pairExists = \list, val, start, end ->
    sum = start.val + end.val

    if sum == val then
        True
    else if end.idx - start.idx < 2 then
        False
    else if sum < val then
        newStart = ListZip.forward start list
        pairExists list val newStart end
    else
        newEnd = ListZip.backward end list
        pairExists list val start newEnd


sortedSubList : List I64, ListZip.Zip, I64 -> List I64
sortedSubList = \list, start, len ->
    list |> ListZip.collect start len |> ListExtra.quicksort


#  second part


weakness : I64, List I64 -> I64
weakness = \inv, list ->
    start = ListZip.newAtFirst list 0
    weaknessHelper1 inv list start


weaknessHelper1 : I64, List I64, ListZip.Zip -> I64
weaknessHelper1 = \inv, list, start ->
    end = ListZip.forward start list
    sum = start.val + end.val
    result = weaknessHelper2 inv list start end sum

    if result > 0 then
        result
    else
        newStart = ListZip.forward start list
        weaknessHelper1 inv list newStart


weaknessHelper2 : I64, List I64, ListZip.Zip, ListZip.Zip, I64 -> I64
weaknessHelper2 = \inv, list, start, end, sum ->
    if sum < inv then
        newEnd = ListZip.forward end list
        newSum = sum + newEnd.val
        weaknessHelper2 inv list start newEnd newSum
    else if sum > inv then
        0
    else
        len = end.idx - start.idx + 1
        sorted = sortedSubList list start len
        first = ListZip.newAtFirst sorted 0
        last = ListZip.last first sorted
        first.val + last.val


#  test data


testInput : List I64
testInput =
    [ 51, 53, 10
    , 50, 48, 10
    , 49, 53, 10
    , 50, 53, 10
    , 52, 55, 10
    , 52, 48, 10
    , 54, 50, 10
    , 53, 53, 10
    , 54, 53, 10
    , 57, 53, 10
    , 49, 48, 50, 10
    , 49, 49, 55, 10
    , 49, 53, 48, 10
    , 49, 56, 50, 10
    , 49, 50, 55, 10
    , 50, 49, 57, 10
    , 50, 57, 57, 10
    , 50, 55, 55, 10
    , 51, 48, 57, 10
    , 53, 55, 54, 10
    ]
