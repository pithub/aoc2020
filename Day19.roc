interface Day19 exposes [ output ] imports [ TestUtil ]


output : List I64 -> List (List I64)
output = \puzzleInput ->
    testData1  = parseData testInput1
    testData2  = parseData testInput2
    puzzleData = parseData puzzleInput

    [ TestUtil.verify 19 1 1 (firstResult testData1 ) 2
    , TestUtil.verify 19 1 2 (firstResult testData2 ) 3
    , TestUtil.show   19 1   (firstResult puzzleData)
    , TestUtil.verify 19 2 1 (secondResult testData2 ) 12
    , TestUtil.show   19 2   (secondResult puzzleData)
    ]


#  first part


firstResult : List I64 -> I64
firstResult = \data ->
    countValidMsgs data (getSafe data 0) 0


countValidMsgs : List I64, I64, I64 -> I64
countValidMsgs = \data, msgIdx, cnt ->
    if msgIdx < List.len data then
        newMsgIdx = isValid data msgIdx
        if newMsgIdx > 0 then
            countValidMsgs data (newMsgIdx + 1) (cnt + 1)
        else
            countValidMsgs data (1 - newMsgIdx) cnt
    else
        cnt


isValid : List I64, I64 -> I64
isValid = \data, msgIdx ->
    eom = nextDelimiter data msgIdx
    afterMatch = match data 0 msgIdx
    if afterMatch == eom then
        eom
    else
        -eom


#  second part


#   8: 42     ->  42{n}         n >= 1
#  11: 42 31  ->  42{n} 31{n}   n >= 1
#  ---
#   0: 8 11  =  42 42 31  ->  42{n+m} 31{m}  n, m >= 1


secondResult : List I64 -> I64
secondResult = \data ->
    countValidMsgs2 data (getSafe data 0) 0


countValidMsgs2 : List I64, I64, I64 -> I64
countValidMsgs2 = \data, msgIdx, cnt ->
    if msgIdx < List.len data then
        newMsgIdx = isValid2 data msgIdx
        if newMsgIdx > 0 then
            countValidMsgs2 data (newMsgIdx + 1) (cnt + 1)
        else
            countValidMsgs2 data (1 - newMsgIdx) cnt
    else
        cnt


isValid2 : List I64, I64 -> I64
isValid2 = \data, msgIdx ->
    eom = nextDelimiter data msgIdx

    newMsgIdx = match data 42 msgIdx
    if newMsgIdx > 0 then
        match2 data eom newMsgIdx 0
    else
        -eom


match2 : List I64, I64, I64, I64 -> I64
match2 = \data, eom, msgIdx, sufLen ->
    newMsgIdx1 = match data 42 msgIdx
    if newMsgIdx1 > 0 then
        newMsgIdx2 = match data 31 newMsgIdx1
        if newMsgIdx2 > 0 then
            afterMatch = matchSuffix data eom newMsgIdx2 sufLen
            if afterMatch == eom then
                eom
            else
                match2 data eom newMsgIdx1 (sufLen + 1)
        else
            match2 data eom newMsgIdx1 (sufLen + 1)
    else
        -eom


matchSuffix : List I64, I64, I64, I64 -> I64
matchSuffix = \data, eom, msgIdx, cnt ->
    if msgIdx == eom then
        eom
    else if cnt > 0 then
        newMsgIdx = match data 31 msgIdx
        if newMsgIdx > 0 then
            matchSuffix data eom newMsgIdx (cnt - 1)
        else
            -eom
    else
        -eom


#  util


match : List I64, I64, I64 -> I64
match = \data, ruleNum, msgIdx ->
    ruleIdx = getSafe data (ruleNum + 1)
    when getSafe data ruleIdx is
        -34 -> matchChar data (ruleIdx + 1) msgIdx
        -58 -> matchSequence data (ruleIdx + 1) msgIdx
        -124 -> matchAlternative data (ruleIdx + 1) msgIdx
        _ -> -msgIdx


matchChar : List I64, I64, I64 -> I64
matchChar = \data, ruleIdx, msgIdx ->
    if getSafe data msgIdx == getSafe data ruleIdx then
        msgIdx + 1
    else
        -msgIdx


matchSequence : List I64, I64, I64 -> I64
matchSequence = \data, ruleIdx, msgIdx ->
    ruleNum = getSafe data ruleIdx
    if ruleNum < 0 then
        msgIdx
    else
        newMsgIdx = match data ruleNum msgIdx
        if newMsgIdx > 0 then
            matchSequence data (ruleIdx + 1) newMsgIdx
        else
            newMsgIdx


matchAlternative : List I64, I64, I64 -> I64
matchAlternative = \data, ruleIdx, msgIdx ->
    newMsgIdx = matchSequence data ruleIdx msgIdx
    if newMsgIdx > 0 then
        newMsgIdx
    else
        newRuleIdx = nextDelimiter data ruleIdx
        if getSafe data newRuleIdx == -124 then
            matchAlternative data (newRuleIdx + 1) msgIdx
        else
            -newRuleIdx


nextDelimiter : List I64, I64 -> I64
nextDelimiter = \data, idx ->
    if getSafe data idx < 0 then
        idx
    else
        nextDelimiter data (idx + 1)


getSafe : List I64, I64 -> I64
getSafe = \list, idx ->
    when List.get list idx is
        Ok val -> val
        _ -> -1


#  parser


parseData : List I64 -> List I64
parseData = \input ->
    parseDataHelper input 0 0 [] 0 []


parseDataHelper : List I64, I64, I64, List I64, I64, List I64 -> List I64
parseDataHelper = \input, idx, num, indexes, start, result ->
    when List.get input idx is
        Ok val ->
            #  new input index
            newIdx = idx + 1

            if 48 <= val && val <= 57 then
                #  add digit to num
                newNum = num * 10 + val - 48
                parseDataHelper input newIdx newNum indexes start result

            else
                #  determine rulesToMessages switch
                rulesToMessages =
                    if val == 10 && List.len result == start then
                        1
                    else
                        0

                #  new rule indexes
                newIndexes =
                    if val == 58 then
                        setSafe indexes num start
                    else
                        indexes

                #  prepend message and rule indexes
                newResult1 =
                    if rulesToMessages > 0 then
                        increase = List.len newIndexes + 1
                        prepend = List.map newIndexes (\i -> i + increase)
                        List.join [ [ increase + List.len result ], prepend, result ]
                    else
                        result

                #  append num if set
                newResult2 =
                    if num > 0 && val != 58 then
                        List.append newResult1 num
                    else
                        newResult1

                #  append val
                newResult3 =
                    if val == 32 || val == 34 || (val == 10 && rulesToMessages > 0) then
                        newResult2
                    else if val == 10 || val == 58 || val == 124 then
                        List.append newResult2 -val
                    else
                        List.append newResult2 val

                #  set rule type
                newResult4 =
                    if val == 34 || val == 124 then
                        List.set newResult3 start -val
                    else
                        newResult3

                #  new start
                newStart =
                    if val == 10 then
                        List.len newResult4
                    else
                        start

                #  recurse with num == 0
                parseDataHelper input newIdx 0 newIndexes newStart newResult4

        _ ->
            #  add final lf
            List.append result -10


setSafe : List I64, I64, I64 -> List I64
setSafe = \list, idx, val ->
    add = idx - List.len list
    if add < 0 then
        List.set list idx val
    else if add > 0 then
        List.join [ list, List.repeat add 0, [ val ] ]
    else
        List.append list val


#  test data


testInput1 : List I64
testInput1 =
    [ 48, 58, 32, 52, 32, 49, 32, 53, 10
    , 49, 58, 32, 50, 32, 51, 32, 124, 32, 51, 32, 50, 10
    , 50, 58, 32, 52, 32, 52, 32, 124, 32, 53, 32, 53, 10
    , 51, 58, 32, 52, 32, 53, 32, 124, 32, 53, 32, 52, 10
    , 52, 58, 32, 34, 97, 34, 10
    , 53, 58, 32, 34, 98, 34, 10
    , 10
    , 97, 98, 97, 98, 98, 98, 10
    , 98, 97, 98, 97, 98, 97, 10
    , 97, 98, 98, 98, 97, 98, 10
    , 97, 97, 97, 98, 98, 98, 10
    , 97, 97, 97, 97, 98, 98, 98
    ]


testInput2 : List I64
testInput2 =
    [ 52, 50, 58, 32, 57, 32, 49, 52, 32, 124, 32, 49, 48, 32, 49, 10
    , 57, 58, 32, 49, 52, 32, 50, 55, 32, 124, 32, 49, 32, 50, 54, 10
    , 49, 48, 58, 32, 50, 51, 32, 49, 52, 32, 124, 32, 50, 56, 32, 49, 10
    , 49, 58, 32, 34, 97, 34, 10
    , 49, 49, 58, 32, 52, 50, 32, 51, 49, 10
    , 53, 58, 32, 49, 32, 49, 52, 32, 124, 32, 49, 53, 32, 49, 10
    , 49, 57, 58, 32, 49, 52, 32, 49, 32, 124, 32, 49, 52, 32, 49, 52, 10
    , 49, 50, 58, 32, 50, 52, 32, 49, 52, 32, 124, 32, 49, 57, 32, 49, 10
    , 49, 54, 58, 32, 49, 53, 32, 49, 32, 124, 32, 49, 52, 32, 49, 52, 10
    , 51, 49, 58, 32, 49, 52, 32, 49, 55, 32, 124, 32, 49, 32, 49, 51, 10
    , 54, 58, 32, 49, 52, 32, 49, 52, 32, 124, 32, 49, 32, 49, 52, 10
    , 50, 58, 32, 49, 32, 50, 52, 32, 124, 32, 49, 52, 32, 52, 10
    , 48, 58, 32, 56, 32, 49, 49, 10
    , 49, 51, 58, 32, 49, 52, 32, 51, 32, 124, 32, 49, 32, 49, 50, 10
    , 49, 53, 58, 32, 49, 32, 124, 32, 49, 52, 10
    , 49, 55, 58, 32, 49, 52, 32, 50, 32, 124, 32, 49, 32, 55, 10
    , 50, 51, 58, 32, 50, 53, 32, 49, 32, 124, 32, 50, 50, 32, 49, 52, 10
    , 50, 56, 58, 32, 49, 54, 32, 49, 10
    , 52, 58, 32, 49, 32, 49, 10
    , 50, 48, 58, 32, 49, 52, 32, 49, 52, 32, 124, 32, 49, 32, 49, 53, 10
    , 51, 58, 32, 53, 32, 49, 52, 32, 124, 32, 49, 54, 32, 49, 10
    , 50, 55, 58, 32, 49, 32, 54, 32, 124, 32, 49, 52, 32, 49, 56, 10
    , 49, 52, 58, 32, 34, 98, 34, 10
    , 50, 49, 58, 32, 49, 52, 32, 49, 32, 124, 32, 49, 32, 49, 52, 10
    , 50, 53, 58, 32, 49, 32, 49, 32, 124, 32, 49, 32, 49, 52, 10
    , 50, 50, 58, 32, 49, 52, 32, 49, 52, 10
    , 56, 58, 32, 52, 50, 10
    , 50, 54, 58, 32, 49, 52, 32, 50, 50, 32, 124, 32, 49, 32, 50, 48, 10
    , 49, 56, 58, 32, 49, 53, 32, 49, 53, 10
    , 55, 58, 32, 49, 52, 32, 53, 32, 124, 32, 49, 32, 50, 49, 10
    , 50, 52, 58, 32, 49, 52, 32, 49, 10
    , 10
    , 97, 98, 98, 98, 98, 98, 97, 98, 98, 98, 97, 97, 97, 97, 98, 97, 98, 98, 97, 97, 98, 98, 98, 98, 97, 98, 97, 98, 97, 98, 98, 98, 97, 98, 98, 98, 98, 98, 98, 97, 98, 97, 97, 97, 97, 10
    , 98, 98, 97, 98, 98, 98, 98, 97, 97, 98, 97, 97, 98, 98, 97, 10
    , 98, 97, 98, 98, 98, 98, 97, 97, 98, 98, 98, 98, 98, 97, 98, 98, 98, 98, 98, 98, 97, 97, 98, 97, 97, 97, 98, 97, 97, 97, 10
    , 97, 97, 97, 98, 98, 98, 98, 98, 98, 97, 97, 97, 97, 98, 97, 97, 98, 97, 98, 97, 97, 98, 97, 98, 97, 98, 98, 97, 98, 97, 97, 97, 98, 98, 97, 98, 97, 98, 97, 98, 97, 98, 97, 97, 97, 10
    , 98, 98, 98, 98, 98, 98, 98, 97, 97, 97, 97, 98, 98, 98, 98, 97, 97, 97, 98, 98, 97, 98, 97, 97, 97, 10
    , 98, 98, 98, 97, 98, 97, 98, 98, 98, 98, 97, 97, 97, 97, 97, 97, 97, 97, 98, 98, 97, 98, 97, 98, 97, 97, 97, 98, 97, 98, 97, 97, 98, 97, 98, 10
    , 97, 98, 97, 98, 97, 97, 97, 97, 97, 97, 98, 97, 97, 97, 98, 10
    , 97, 98, 97, 98, 97, 97, 97, 97, 97, 98, 98, 98, 97, 98, 97, 10
    , 98, 97, 97, 98, 98, 97, 97, 97, 97, 98, 98, 97, 97, 97, 97, 98, 97, 98, 98, 97, 97, 98, 97, 98, 98, 10
    , 97, 98, 98, 98, 98, 97, 98, 98, 98, 98, 97, 97, 97, 97, 98, 97, 98, 98, 98, 98, 98, 98, 97, 97, 97, 97, 98, 97, 98, 98, 10
    , 97, 97, 97, 97, 97, 98, 98, 97, 97, 98, 97, 97, 97, 97, 97, 98, 97, 98, 97, 97, 10
    , 97, 97, 97, 97, 98, 98, 97, 97, 97, 97, 98, 98, 97, 97, 97, 10
    , 97, 97, 97, 97, 98, 98, 97, 97, 98, 98, 97, 97, 97, 97, 97, 97, 97, 98, 98, 98, 97, 98, 98, 98, 97, 97, 97, 98, 98, 97, 97, 98, 97, 97, 97, 10
    , 98, 97, 98, 97, 97, 97, 98, 98, 98, 97, 97, 97, 98, 97, 97, 98, 97, 98, 98, 97, 97, 98, 97, 98, 97, 98, 97, 97, 97, 98, 10
    , 97, 97, 98, 98, 98, 98, 98, 97, 97, 98, 98, 98, 97, 97, 97, 97, 97, 97, 98, 98, 98, 98, 98, 97, 98, 97, 98, 97, 97, 97, 97, 97, 98, 98, 97, 97, 97, 98, 98, 97
    ]
