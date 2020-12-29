interface Day16 exposes [ output ] imports [ ListZip, TestUtil ]


output : List I64 -> List (List I64)
output = \puzzleInput ->
    testData1  = parseData testInput1
    testData2  = parseData testInput2
    puzzleData = parseData puzzleInput

    [ TestUtil.verify 16 1 1 (errorRate testData1 ) 71
    , TestUtil.show   16 1   (errorRate puzzleData)
    , TestUtil.verify 16 2 1 (fieldProduct 1 2 testData2 ) 143
    , TestUtil.show   16 2   (fieldProduct 0 5 puzzleData)
    ]


#  first part


errorRate : List I64 -> I64
errorRate = \data ->
    maxFieldStart = getMyTicketIdx data
    ticketIdx = getNearbyTicketIdx data
    errorRateHelper data maxFieldStart ticketIdx 0


errorRateHelper : List I64, I64, I64, I64 -> I64
errorRateHelper = \data, maxFieldStart, ticketIdx, rate ->
    when List.get data ticketIdx is
        Ok ticketVal ->
            newTicketIdx = ticketIdx + 1
            newRate = rate + ticketVal * (errorFactor data ticketVal maxFieldStart 1)
            errorRateHelper data maxFieldStart newTicketIdx newRate
        _ ->
            rate


#  second part


fieldProduct : I64, I64, List I64 -> I64
fieldProduct = \minFieldNum, maxFieldNum, data ->
    fieldCnt = getFieldCnt data
    possibilities = determinePossibilities data fieldCnt

    myTicketIdx = getMyTicketIdx data
    productHelper data myTicketIdx possibilities maxFieldNum minFieldNum 1


determinePossibilities : List I64, I64 -> List I64
determinePossibilities = \data, fieldCnt ->
    maxFieldStart = getMyTicketIdx data
    ticketIdx = getNearbyTicketIdx data
    validTicketIndexes = determineValidTicketIndexes data fieldCnt maxFieldStart ticketIdx 0 []

    initialPossibilities = List.repeat (fieldCnt * fieldCnt) 1
    finalPossibilities = determinePossibilitiesHelper data fieldCnt validTicketIndexes initialPossibilities 0

    collectFieldIndexes finalPossibilities fieldCnt 0 []


determineValidTicketIndexes : List I64, I64, I64, I64, I64, List I64 -> List I64
determineValidTicketIndexes = \data, fieldCnt, maxFieldStart, ticketIdx, fieldIdx, result ->
    when List.get data (ticketIdx + fieldIdx) is
        Ok ticketVal ->
            if errorFactor data ticketVal maxFieldStart 1 > 0 then
                newTicketIdx = ticketIdx + fieldCnt
                determineValidTicketIndexes data fieldCnt maxFieldStart newTicketIdx 0 result
            else
                newFieldIdx = fieldIdx + 1
                if newFieldIdx < fieldCnt then
                    determineValidTicketIndexes data fieldCnt maxFieldStart ticketIdx newFieldIdx result
                else
                    newTicketIdx = ticketIdx + fieldCnt
                    newResult = List.append result ticketIdx
                    determineValidTicketIndexes data fieldCnt maxFieldStart newTicketIdx 0 newResult
        _ ->
            result


determinePossibilitiesHelper : List I64, I64, List I64, List I64, I64 -> List I64
determinePossibilitiesHelper = \data, fieldCnt, validTicketIndexes, possibilities, possIdx ->
    when List.get possibilities possIdx is
        Ok possible ->
            newPossIdx = possIdx + 1
            newPossibilities =
                if possible > 0 then
                    when possIdx // fieldCnt is
                        Ok fieldNum ->
                            fieldStart = 1 + 4 * fieldNum
                            fieldIdx = possIdx - fieldCnt * fieldNum
                            if determinePossibility data fieldStart fieldIdx validTicketIndexes 0 > 0 then
                                possibilities
                            else
                                removePossibility possibilities fieldCnt fieldNum fieldIdx
                        _ ->
                            possibilities
                else
                    possibilities
            determinePossibilitiesHelper data fieldCnt validTicketIndexes newPossibilities newPossIdx
        _ ->
            possibilities


determinePossibility : List I64, I64, I64, List I64, I64 -> I64
determinePossibility = \data, fieldStart, fieldIdx, validTicketIndexes, validTicketIdx ->
    when List.get validTicketIndexes validTicketIdx is
        Ok ticketIdx ->
            valIdx = ticketIdx + fieldIdx
            when List.get data valIdx is
                Ok ticketVal ->
                    if valError data ticketVal fieldStart > 0 then
                        0
                    else
                        newValidTicketIdx = validTicketIdx + 1
                        determinePossibility data fieldStart fieldIdx validTicketIndexes newValidTicketIdx
                _ ->
                    0
        _ ->
            1


removePossibility : List I64, I64, I64, I64 -> List I64
removePossibility = \possibilities, fieldCnt, fieldNum, fieldIdx ->
    possIdx = fieldNum * fieldCnt + fieldIdx
    newPossibilities1 = List.set possibilities possIdx 0

    newPossibilities2 =
        when getSingleFieldNum newPossibilities1 fieldCnt fieldIdx 0 -1 is
            Ok singleFieldNum ->
                removeOtherFieldIdxs newPossibilities1 fieldCnt singleFieldNum fieldIdx 0
            _ ->
                newPossibilities1

    when getSingleFieldIdx newPossibilities2 fieldCnt fieldNum 0 -1 is
        Ok singleFieldIdx ->
            removeOtherFieldNums newPossibilities2 fieldCnt singleFieldIdx fieldNum 0
        _ ->
            newPossibilities2


collectFieldIndexes : List I64, I64, I64, List I64 -> List I64
collectFieldIndexes = \possibilities, fieldCnt, fieldNum, result ->
    if fieldNum < fieldCnt then
        when getSingleFieldIdx possibilities fieldCnt fieldNum 0 -1 is
            Ok fieldIdx ->
                newFieldNum = fieldNum + 1
                newResult = List.append result fieldIdx
                collectFieldIndexes possibilities fieldCnt newFieldNum newResult
            _ ->
                result
    else
        result


getSingleFieldNum : List I64, I64, I64, I64, I64 -> Result I64 I64
getSingleFieldNum = \possibilities, fieldCnt, fieldIdx, fieldNum, result ->
    if fieldNum < fieldCnt then
        possIdx = fieldNum * fieldCnt + fieldIdx
        when List.get possibilities possIdx is
            Ok possibility ->
                newFieldNum = fieldNum + 1
                if possibility > 0 then
                    if result < 0 then
                        newResult = fieldNum + 0  # compiler error
                        getSingleFieldNum possibilities fieldCnt fieldIdx newFieldNum newResult
                    else
                        Err fieldIdx
                else
                    getSingleFieldNum possibilities fieldCnt fieldIdx newFieldNum result
            _ ->
                Err fieldIdx
    else if result < 0 then
        Err fieldIdx
    else
        Ok result


getSingleFieldIdx : List I64, I64, I64, I64, I64 -> Result I64 I64
getSingleFieldIdx = \possibilities, fieldCnt, fieldNum, fieldIdx, result ->
    if fieldIdx < fieldCnt then
        possIdx = fieldNum * fieldCnt + fieldIdx
        when List.get possibilities possIdx is
            Ok possibility ->
                newFieldIdx = fieldIdx + 1
                if possibility > 0 then
                    if result < 0 then
                        newResult = fieldIdx + 0  # compiler error
                        getSingleFieldIdx possibilities fieldCnt fieldNum newFieldIdx newResult
                    else
                        Err fieldNum
                else
                    getSingleFieldIdx possibilities fieldCnt fieldNum newFieldIdx result
            _ ->
                Err fieldNum
    else if result < 0 then
        Err fieldNum
    else
        Ok result


removeOtherFieldNums : List I64, I64, I64, I64, I64 -> List I64
removeOtherFieldNums = \possibilities, fieldCnt, fieldIdx, singleFieldNum, fieldNum ->
    if fieldNum < fieldCnt then
        newFieldNum = fieldNum + 1
        newPossibilities =
            if fieldNum == singleFieldNum then
                possibilities
            else
                possIdx = fieldNum * fieldCnt + fieldIdx
                when List.get possibilities possIdx is
                    Ok possibility ->
                        if possibility > 0 then
                            removePossibility possibilities fieldCnt fieldNum fieldIdx
                        else
                            possibilities
                    _ ->
                        possibilities
        removeOtherFieldNums newPossibilities fieldCnt fieldIdx singleFieldNum newFieldNum
    else
        possibilities


removeOtherFieldIdxs : List I64, I64, I64, I64, I64 -> List I64
removeOtherFieldIdxs = \possibilities, fieldCnt, fieldNum, singleFieldIdx, fieldIdx ->
    if fieldIdx < fieldCnt then
        newFieldIdx = fieldIdx + 1
        newPossibilities =
            if fieldIdx == singleFieldIdx then
                possibilities
            else
                possIdx = fieldNum * fieldCnt + fieldIdx
                when List.get possibilities possIdx is
                    Ok possibility ->
                        if possibility > 0 then
                            removePossibility possibilities fieldCnt fieldNum fieldIdx
                        else
                            possibilities
                    _ ->
                        possibilities
        removeOtherFieldIdxs newPossibilities fieldCnt fieldNum singleFieldIdx newFieldIdx
    else
        possibilities


productHelper : List I64, I64, List I64, I64, I64, I64 -> I64
productHelper = \data, myTicketIdx, possibilities, maxFieldNum, fieldNum, result ->
    if fieldNum > maxFieldNum then
        result
    else
        when List.get possibilities fieldNum is
            Ok fieldIdx ->
                when List.get data (myTicketIdx + fieldIdx) is
                    Ok ticketVal ->
                        newFieldNum = fieldNum + 1
                        newResult = result * ticketVal
                        productHelper data myTicketIdx possibilities maxFieldNum newFieldNum newResult
                    _ ->
                        0
            _ ->
                0


#  utils


getFieldCnt : List I64 -> I64
getFieldCnt = \data ->
    safeGet data 0


getMyTicketIdx : List I64 -> I64
getMyTicketIdx = \data ->
    1 + (getFieldCnt data) * 4


getNearbyTicketIdx : List I64 -> I64
getNearbyTicketIdx = \data ->
    1 + (getFieldCnt data) * 5


errorFactor : List I64, I64, I64, I64 -> I64
errorFactor = \data, ticketVal, maxFieldStart, fieldStart ->
    if fieldStart < maxFieldStart then
        if valError data ticketVal fieldStart > 0 then
            newFieldIdx = fieldStart + 4
            errorFactor data ticketVal maxFieldStart newFieldIdx
        else
            0
    else
        1


valError : List I64, I64, I64 -> I64
valError = \data, ticketVal, fieldStart ->
    min1 = safeGet data (fieldStart + 0)
    max1 = safeGet data (fieldStart + 1)
    min2 = safeGet data (fieldStart + 2)
    max2 = safeGet data (fieldStart + 3)
    if (min1 <= ticketVal && ticketVal <= max1) || (min2 <= ticketVal && ticketVal <= max2) then
        0
    else
        1


safeGet : List I64, I64 -> I64
safeGet = \list, idx ->
    when List.get list idx is
        Ok n -> n
        _ -> 0


#  parser


parseData : List I64 -> List I64
parseData = \input ->
    zip = ListZip.newAtFirst input 0

    fieldInts = parseFieldInts zip input [ 0 ]
    fieldCnt =
        when (List.len fieldInts.val - 1) // 4 is
            Ok n -> n
            _ -> 0
    fieldIntsWithFieldCnt = List.set fieldInts.val 0 fieldCnt

    parseTicketInts fieldInts.zip input fieldIntsWithFieldCnt


Res a : { zip : ListZip.Zip, val : a }


parseFieldInts : ListZip.Zip, List I64, List I64 -> Res (List I64)
parseFieldInts = \zip, input, result ->
    if zip.val == 10 then
        { zip, val: result }
    else
        min1 = parseInt zip input
        max1 = parseInt min1.zip input
        min2 = parseInt max1.zip input
        max2 = parseInt min2.zip input
        eol = ListZip.forward max2.zip input

        newResult = List.concat result [ min1.val, max1.val, min2.val, max2.val ]
        parseFieldInts eol input newResult


parseTicketInts : ListZip.Zip, List I64, List I64 -> List I64
parseTicketInts = \zip, input, result ->
    if ListZip.afterLast zip then
        result
    else
        res = parseInt zip input
        newResult = List.append result res.val
        parseTicketInts res.zip input newResult


parseInt : ListZip.Zip, List I64 -> Res I64
parseInt = \zip, input ->
    digit = moveToDigit zip input
    parseIntHelper digit input 0


moveToDigit : ListZip.Zip, List I64 -> ListZip.Zip
moveToDigit = \zip, input ->
    if 48 <= zip.val && zip.val <= 57 then
        zip
    else
        newZip = ListZip.forward zip input
        moveToDigit newZip input


parseIntHelper : ListZip.Zip, List I64, I64 -> Res I64
parseIntHelper = \zip, input, num ->
    newZip = ListZip.forward zip input
    newNum = num * 10 + zip.val - 48
    if 48 <= newZip.val && newZip.val <= 57 then
        parseIntHelper newZip input newNum
    else
        { zip: newZip, val: newNum }


#  test data


testInput1 : List I64
testInput1 =
    [ 99, 108, 97, 115, 115, 58, 32, 49, 45, 51, 32, 111, 114, 32, 53, 45, 55, 10
    , 114, 111, 119, 58, 32, 54, 45, 49, 49, 32, 111, 114, 32, 51, 51, 45, 52, 52, 10
    , 115, 101, 97, 116, 58, 32, 49, 51, 45, 52, 48, 32, 111, 114, 32, 52, 53, 45, 53, 48, 10
    , 10
    , 121, 111, 117, 114, 32, 116, 105, 99, 107, 101, 116, 58, 10
    , 55, 44, 49, 44, 49, 52, 10
    , 10
    , 110, 101, 97, 114, 98, 121, 32, 116, 105, 99, 107, 101, 116, 115, 58, 10
    , 55, 44, 51, 44, 52, 55, 10
    , 52, 48, 44, 52, 44, 53, 48, 10
    , 53, 53, 44, 50, 44, 50, 48, 10
    , 51, 56, 44, 54, 44, 49, 50
    ]


testInput2 : List I64
testInput2 =
    [ 99, 108, 97, 115, 115, 58, 32, 48, 45, 49, 32, 111, 114, 32, 52, 45, 49, 57, 10
    , 114, 111, 119, 58, 32, 48, 45, 53, 32, 111, 114, 32, 56, 45, 49, 57, 10
    , 115, 101, 97, 116, 58, 32, 48, 45, 49, 51, 32, 111, 114, 32, 49, 54, 45, 49, 57, 10
    , 10
    , 121, 111, 117, 114, 32, 116, 105, 99, 107, 101, 116, 58, 10
    , 49, 49, 44, 49, 50, 44, 49, 51, 10
    , 10
    , 110, 101, 97, 114, 98, 121, 32, 116, 105, 99, 107, 101, 116, 115, 58, 10
    , 51, 44, 57, 44, 49, 56, 10
    , 49, 53, 44, 49, 44, 53, 10
    , 53, 44, 49, 52, 44, 57
    ]
