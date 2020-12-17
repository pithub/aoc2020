interface Day15 exposes [ output ] imports [ ListTree, TestUtil ]


output : List I64 -> List (List I64)
output = \puzzleInput ->
    testData1   = parseData testInput1
    testData2   = parseData testInput2
    testData3   = parseData testInput3
    testData4   = parseData testInput4
    testData5   = parseData testInput5
    testData6   = parseData testInput6
    testData7   = parseData testInput7
    puzzleData = parseData puzzleInput

    [ TestUtil.verify 15 1 1 (firstResult testData1 ) 436
    #, TestUtil.verify 15 1 2 (firstResult testData2 ) 1
    #, TestUtil.verify 15 1 3 (firstResult testData3 ) 10
    #, TestUtil.verify 15 1 4 (firstResult testData4 ) 27
    #, TestUtil.verify 15 1 5 (firstResult testData5 ) 78
    #, TestUtil.verify 15 1 6 (firstResult testData6 ) 438
    #, TestUtil.verify 15 1 7 (firstResult testData7 ) 1836
    #, TestUtil.show   15 1   (firstResult puzzleData)
    #, TestUtil.verify 15 2 1 (secondResult testData1  ) 175594
    #, TestUtil.verify 15 2 2 (secondResult testData2  ) 2578
    #, TestUtil.verify 15 2 3 (secondResult testData3  ) 3544142
    #, TestUtil.verify 15 2 4 (secondResult testData4  ) 261214
    #, TestUtil.verify 15 2 5 (secondResult testData5  ) 6895259
    #, TestUtil.verify 15 2 6 (secondResult testData6  ) 18
    #, TestUtil.verify 15 2 7 (secondResult testData7  ) 362
    #, TestUtil.show   15 2   (secondResult puzzleData)
    ]


#  first part


firstResult : List I64 -> I64
firstResult = \data ->
    last = getStart data 0 []
    turn = List.len data
    idx = List.len data - 1
    num = read data idx
    firstResultHelper last turn num 0


firstResultHelper : List I64, I64, I64, I64 -> I64
firstResultHelper = \last, turn, num, lastTurn ->
    if turn < 300000 then
        newTurn = turn + 1
        newNum =
            if lastTurn > 0 then
                turn - lastTurn
            else
                0
        newLastTurn = read last newNum
        newLast = addLast last newNum newTurn
        firstResultHelper newLast newTurn newNum newLastTurn
    else
        num


getStart : List I64, I64, List I64 -> List I64
getStart = \nums, idx, result ->
    num = read nums idx
    if num < 0 then
        result
    else
        newIdx = idx + 1
        turn = idx + 1
        newResult = addLast result num turn
        getStart nums newIdx newResult


addLast : List I64, I64, I64 -> List I64
addLast = \last, num, turn ->
    last |> ensureSize num |> List.set num turn


ensureSize : List I64, I64 -> List I64
ensureSize = \list, len ->
    if List.len list <= len then
        newList = List.append list 0
        ensureSize newList len
    else
        list


read : List I64, I64 -> I64
read = \list, idx ->
    when List.get list idx is
        Ok n -> n
        _ -> -1


#  second part


secondResult : List I64 -> I64
secondResult = \data ->
    initialTree = ListTree.empty
    tree = getStartTree data 0 initialTree
    turn = List.len data
    idx = List.len data - 1
    num = read data idx
    secondResultHelper tree turn num 0


getStartTree : List I64, I64, List I64 -> List I64
getStartTree = \nums, idx, tree ->
    num = read nums idx
    if num < 0 then
        tree
    else
        newIdx = idx + 1
        newTree = ListTree.insert tree num newIdx
        getStartTree nums newIdx newTree


secondResultHelper : List I64, I64, I64, I64 -> I64
secondResultHelper = \last, turn, num, lastTurn ->
    if turn < 300000 then
        newTurn = turn + 1
        newNum =
            if lastTurn > 0 then
                turn - lastTurn
            else
                0
        newLastTurn =
            when ListTree.get last newNum is
                Ok n -> n
                _ -> 0
        newLast = ListTree.insert last newNum newTurn
        secondResultHelper newLast newTurn newNum newLastTurn
    else
        num


#      3000  0.5
#     30000  3.0
#    300000


#  test data


parseData : List I64 -> List I64
parseData = \input ->
    initial = { num : 0, nums: [] }
    final = List.walk input parseWalker initial
    List.append final.nums final.num


ParseAcc : { num : I64, nums : List I64 }


parseWalker : I64, ParseAcc -> ParseAcc
parseWalker = \val, acc ->
    if val == 44 then
        { num: 0, nums: List.append acc.nums acc.num }
    else
        { acc & num: 10 * acc.num + val - 48 }


testInput1 : List I64
testInput1 =
    [ 48, 44, 51, 44, 54 ]


testInput2 : List I64
testInput2 =
    [ 49, 44, 51, 44, 50 ]


testInput3 : List I64
testInput3 =
    [ 50, 44, 49, 44, 51 ]


testInput4 : List I64
testInput4 =
    [ 49, 44, 50, 44, 51 ]


testInput5 : List I64
testInput5 =
    [ 50, 44, 51, 44, 49 ]


testInput6 : List I64
testInput6 =
    [ 51, 44, 50, 44, 49 ]


testInput7 : List I64
testInput7 =
    [ 51, 44, 49, 44, 50 ]
