interface Day07 exposes [ output ] imports [ TestUtil ]


output : List I64 -> List (List I64)
output = \puzzleInput ->
    testMatrix1  = buildMatrix  10 testInput1
    testMatrix2  = buildMatrix   8 testInput2
    puzzleMatrix = buildMatrix 595 puzzleInput

    [ TestUtil.verify 7 1 1 (outerBags   8  10 testMatrix1 ) 4
    , TestUtil.show   7 1   (outerBags 474 595 puzzleMatrix)
    , TestUtil.verify 7 2 1 (innerBags   8  10 testMatrix1 ) 32
    , TestUtil.verify 7 2 2 (innerBags   1   8 testMatrix2 ) 126
    , TestUtil.show   7 2   (innerBags 474 595 puzzleMatrix)
    ]


# count outer bags


outerBags : I64, I64, List I64 -> I64
outerBags = \num, len, matrix ->
    counted = List.repeat len 0
    outerBagsHelper len matrix counted [ num ] num 0 [] 1


outerBagsHelper : I64, List I64, List I64, List I64, I64, I64, List I64, I64 -> I64
outerBagsHelper = \len, matrix, counted, current, curVal, curIdx, next, parent ->
    if parent < len then
        newParent = parent + 1
        idx = index len curVal parent
        when List.get matrix idx is
            Ok count ->
                if count > 0 then
                    when List.get counted parent is
                        Ok 0 ->
                            newCounted = List.set counted parent 1
                            newNext = List.append next parent
                            outerBagsHelper len matrix newCounted current curVal curIdx newNext newParent
                        _ ->
                            outerBagsHelper len matrix counted current curVal curIdx next newParent
                else
                    outerBagsHelper len matrix counted current curVal curIdx next newParent
            _ ->
                outerBagsHelper len matrix counted current curVal curIdx next newParent
    else
        newCurIdx = curIdx + 1
        when List.get current newCurIdx is
            Ok newCurVal ->
                outerBagsHelper len matrix counted current newCurVal newCurIdx next 1
            _ ->
                when List.get next 0 is
                    Ok newCurVal ->
                        outerBagsHelper len matrix counted next newCurVal 0 [] 1
                    _ ->
                        List.sum counted


# count inner bags


innerBags : I64, I64, List I64 -> I64
innerBags = \num, len, matrix ->
    current = List.repeat len 0
    next = List.repeat len 0
    innerBagsHelper len matrix 0 current 1 num next 1


innerBagsHelper : I64, List I64, I64, List I64, I64, I64, List I64, I64 -> I64
innerBagsHelper = \len, matrix, total, current, curVal, curIdx, next, child ->
    if child < len then
        newChild = child + 1
        idx = index len child curIdx
        when List.get matrix idx is
            Ok count ->
                childCount = count * curVal
                newTotal = total + childCount
                newNext = addCount next child childCount
                innerBagsHelper len matrix newTotal current curVal curIdx newNext newChild
            _ ->
                innerBagsHelper len matrix total current curVal curIdx next newChild
    else
        newCurIdx = curIdx + 1
        when List.get current newCurIdx is
            Ok newCurVal ->
                if newCurVal > 0 then
                    innerBagsHelper len matrix total current newCurVal newCurIdx next 1
                else
                    innerBagsHelper len matrix total current newCurVal newCurIdx next child
            _ ->
                if List.sum next > 0 then
                    when List.get next 1 is
                        Ok newCurVal ->
                            newNext = List.repeat len 0
                            innerBagsHelper len matrix total next newCurVal 1 newNext 1
                        _ ->
                            total
                else
                    total


addCount : List I64, I64, I64 -> List I64
addCount = \counts, idx, val ->
    when List.get counts idx is
        Ok oldCount ->
            List.set counts idx (oldCount + val)
        _ ->
            counts


# create N:M container matrix


buildMatrix : I64, List I64 -> List I64
buildMatrix = \len, input ->
    initial =
        { current: 0
        , outer: 0
        , count: 0
        , parents: List.repeat (len * len) 0
        , length: len
        }
    final = List.walk input walker initial
    final.parents


MatrixAcc : { current : I64, outer : I64, count : I64, parents : List I64, length : I64 }


walker : I64, MatrixAcc -> MatrixAcc
walker = \val, acc ->
    when val is
        58 ->
            newOuter = acc.current
            { acc & current: 0, outer: newOuter }

        124 ->
            newCount = acc.current
            { acc & current: 0, count: newCount }

        60 ->
            idx = index acc.length acc.current acc.outer
            newParents = List.set acc.parents idx acc.count
            { acc & current: 0, count: 0, parents: newParents }

        10 ->
            { acc & outer: 0 }

        _ ->
            newCurrent = 10 * acc.current + val - 48
            { acc & current: newCurrent }


index : I64, I64, I64 -> I64
index = \len, inner, outer ->
    inner * len + outer


#  test data


testInput1 : List I64
testInput1 =
    [ 49, 58, 49, 124, 56, 60, 10
    , 50, 58, 51, 124, 53, 60, 52, 124, 52, 60, 10
    , 51, 58, 51, 124, 49, 60, 52, 124, 55, 60, 10
    , 52, 58, 10
    , 53, 58, 10
    , 54, 58, 49, 124, 49, 60, 50, 124, 55, 60, 10
    , 55, 58, 50, 124, 56, 60, 57, 124, 53, 60, 10
    , 56, 58, 49, 124, 50, 60, 50, 124, 57, 60, 10
    , 57, 58, 53, 124, 53, 60, 54, 124, 52, 60, 10
    ]


testInput2 : List I64
testInput2 =
    [ 49, 58, 50, 124, 50, 60, 10
    , 50, 58, 50, 124, 51, 60, 10
    , 51, 58, 50, 124, 52, 60, 10
    , 52, 58, 50, 124, 53, 60, 10
    , 53, 58, 50, 124, 54, 60, 10
    , 54, 58, 50, 124, 55, 60, 10
    , 55, 58, 10
    ]
