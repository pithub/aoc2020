interface Day13 exposes [ output ] imports [ ListZip, TestUtil ]


output : List I64 -> List (List I64)
output = \puzzleInput ->
    testData1  = parseData testInput1
    testData2  = parseData testInput2
    testData3  = parseData testInput3
    testData4  = parseData testInput4
    testData5  = parseData testInput5
    testData6  = parseData testInput6
    puzzleData = parseData puzzleInput

    [ TestUtil.verify 13 1 1 (firstResult testData1 ) 295
    , TestUtil.show   13 1   (firstResult puzzleData)
    , TestUtil.verify 13 2 1 (secondResult testData1 ) 1068781
    , TestUtil.verify 13 2 2 (secondResult testData2 ) 3417
    , TestUtil.verify 13 2 3 (secondResult testData3 ) 754018
    , TestUtil.verify 13 2 4 (secondResult testData4 ) 779210
    , TestUtil.verify 13 2 5 (secondResult testData5 ) 1261476
    , TestUtil.verify 13 2 6 (secondResult testData6 ) 1202161486
    , TestUtil.show   13 2   (secondResult puzzleData)
    ]


#  first part


firstResult : List I64 -> I64
firstResult = \data ->
    zip = ListZip.newAtFirst data 0
    desiredTime = zip.val
    buses = ListZip.forward zip data
    waitHelper desiredTime buses data 0 0


waitHelper : I64, ListZip.Zip, List I64, I64, I64 -> I64
waitHelper = \desiredTime, buses, data, minBus, minWait ->
    if ListZip.afterLast buses then
        minBus * minWait
    else
        newBuses = ListZip.forward buses data
        if buses.val > 0 then
            newWait = waitTime desiredTime buses.val
            if newWait < minWait || minBus == 0 then
                waitHelper desiredTime newBuses data buses.val newWait
            else
                waitHelper desiredTime newBuses data minBus minWait
        else
            waitHelper desiredTime newBuses data minBus minWait


waitTime : I64, I64 -> I64
waitTime = \time, bus ->
    when time % bus is
        Ok n ->
            if n == 0 then
                0
            else
                bus - n
        _ ->
            -1


#  second part


#  17.a = 13.b - 2 = 19.c - 3
#  --
#  17 = 13.1 + 4  |  4 = 17.1 - 13.1                 |  17.1 = 13.1 + 4
#  13 = 4.3 + 1   |  1 = 13.1 - 4.3 = 17.-3 - 13.-4  |  17.3 = 13.4 - 1
#
#  -2 = 17.6 - 13.8                                  |  17.6 = 13.8 - 2
#
#  17.(6 + 13.a) = 13.(8 + 17.a) - 2
#  --
#  17.6 + 17.13.a = 19.c - 3
#  221.a = 19.c - 105
#
#  221 = 19.11 + 12  |  12 = 221.1 - 19.11                 |  221.1 = 19.11 + 12
#  19 = 12.1 + 7     |  -7 = 12.1 - 19.1 = 221.1 - 19.12   |  221.1 = 19.12 - 7
#  12 = 7.1 + 5      |  5 = 12.1 - 7.1 = 221.2 - 19.23     |  221.2 = 19.23 + 5
#                    |  -105 = 221.-42 - 19.-483           |
#                    |       = 221.15 - 19.180             |  221.15 = 19.180 - 105
#
#  17.(6 + 13.15) = 13.(8 + 17.15) - 2 = 19.180 - 3 = 3417
#  --
#  0, 17, 13, -2 -> 102, 221
#  102, 221, 19, -3 -> 3417, 4199


#  17.a = 13.b - 2 = 19.c - 3
#  17.a + 3 = 13.b + 1 = 19.c + 0
#  --
#  n mod 17 = 3  |   0
#  n mod 13 = 1  |  11
#  n mod 19 = 0  |  16
#  --
#       1   3
#  17  13   4   1
#   1   0   1  -3
#   0   1  -1   4
#  --
#  17.-3 + 13.4 = 1
#  1.17.-3 + 3.13.4 = -51 + 156 = 105  |  11.17.-3 = -561 == 102
#  n mod 221 = 105
#  --
#       11    1   1    1   2
#  221  19   12   7    5   2    1
#    1   0    1  -1    2  -3    8
#    0   1  -11  12  -23  35  -93
#  --
#  221.8 + 19.-93 = 1
#  0.221.8 + 105.19.-93 = -185535 == 3420 (4199)  |  16.221.8 + 102.19.-93 = -151946 == 3417
#            105.-93 == 180 (221)
#  --
#   17,   0, 13, 11 ->  221,  102
#  221, 102, 19, 16 -> 4199, 3417


secondResult : List I64 -> I64
secondResult = \data ->
    2 * List.len data


#  test data


parseData : List I64 -> List I64
parseData = \input ->
    zip = ListZip.newAtFirst input 0
    parseHelper zip input 0 []


parseHelper : ListZip.Zip, List I64, I64, List I64 -> List I64
parseHelper = \zip, input, num, result ->
    if ListZip.afterLast zip then
        List.append result num
    else
        newZip = ListZip.forward zip input
        if 48 <= zip.val && zip.val <= 57 then
            newNum = 10 * num + zip.val - 48
            parseHelper newZip input newNum result
        else if zip.val == 120 then
            parseHelper newZip input 0 result
        else
            newResult = List.append result num
            parseHelper newZip input 0 newResult


testInput1 : List I64
testInput1 =
    [ 57, 51, 57, 10
    , 55, 44, 49, 51, 44, 120, 44, 120, 44, 53, 57, 44, 120, 44, 51, 49, 44, 49, 57
    ]


testInput2 : List I64
testInput2 =
    [ 49, 10
    , 49, 55, 44, 120, 44, 49, 51, 44, 49, 57
    ]


testInput3 : List I64
testInput3 =
    [ 49, 10
    , 54, 55, 44, 55, 44, 53, 57, 44, 54, 49
    ]


testInput4 : List I64
testInput4 =
    [ 49, 10
    , 54, 55, 44, 120, 44, 55, 44, 53, 57, 44, 54, 49
    ]


testInput5 : List I64
testInput5 =
    [ 49, 10
    , 54, 55, 44, 55, 44, 120, 44, 53, 57, 44, 54, 49
    ]


testInput6 : List I64
testInput6 =
    [ 49, 10
    , 49, 55, 56, 57, 44, 51, 55, 44, 52, 55, 44, 49, 56, 56, 57
    ]
