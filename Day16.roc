interface Day16 exposes [ output ] imports [ ListZip ]


output : List I64 -> List (List I64)
output = \puzzleInput ->
    testData1  = parseData testInput1
    testData2  = parseData testInput2
    puzzleData = parseData puzzleInput

    #[ TestUtil.verify 16 1 1 (errorRate testData1 ) 71
    #, TestUtil.show   16 1   (errorRate puzzleData)
    #, TestUtil.verify 16 2 1 (fieldProduct 1 2 testData2 ) 143
    #, TestUtil.show   16 2   (fieldProduct 0 5 puzzleData)
    #]

    fieldProduct 1 2 testData2
    #fieldProduct 0 5 puzzleData


Field : { idx : I64, min1 : I64, max1 : I64, min2 : I64, max2 : I64 }

Ticket : List I64

Notes : { fields : List Field, mine : Ticket, nearby : List Ticket }


#  first part


errorRate : Notes -> I64
errorRate = \notes ->
    sumOfInvalidValues notes.nearby notes.fields 0 0


sumOfInvalidValues : List Ticket, List Field, I64, I64 -> I64
sumOfInvalidValues = \tickets, fields, idx, result ->
    when List.get tickets idx is
        Ok ticket ->
            newIdx = idx + 1
            newResult = result + (sumOfInvalidTicketValues ticket fields 0 0)
            sumOfInvalidValues tickets fields newIdx newResult
        _ ->
            result


#  second part


#  todo noch mehr highlevel programmieren, nicht schon vorher optimieren
#  schleife über alle kombinationen: wenn noch möglich, prüfen, dann ev entfernen


fieldProduct : I64, I64, Notes -> List (List I64)
fieldProduct = \_fldMin, _fldMax, notes ->
    validTickets = validTicketsHelper notes.nearby notes.fields 0 []
    validNotes = { notes & nearby: validTickets }

    initial = allPossibilities notes.fields
    final = processTicketField initial validNotes 0 0

    List.join
        [ initial.nums, [ initial.cnts ]
        , final.nums, [ final.cnts ]
        ]


validTicketsHelper : List Ticket, List Field, I64, List Ticket -> List Ticket
validTicketsHelper = \tickets, fields, idx, result ->
    when List.get tickets idx is
        Ok ticket ->
            newIdx = idx + 1
            newResult =
                if ticketIsValid ticket fields then
                    List.join [ result, [ ticket] ]
                else
                    result
            validTicketsHelper tickets fields newIdx newResult
        _ ->
            result


ticketIsValid : Ticket, List Field -> Bool
ticketIsValid = \ticket, fields ->
    sumOfInvalidTicketValues ticket fields 0 0 == 0


Possibilities : { nums : List (List I64), cnts : List I64 }


allPossibilities : List Field -> Possibilities
allPossibilities = \fields ->
    allFieldNums = List.map fields (\field -> field.idx)
    fieldCnt = List.len allFieldNums
    { nums: List.repeat fieldCnt allFieldNums, cnts: List.repeat fieldCnt fieldCnt }


processTicketField : Possibilities, Notes, I64, I64 -> Possibilities
processTicketField = \possibilities, notes, ticketFieldNum, fieldNumIdx ->
    when List.get possibilities.nums ticketFieldNum is
        Ok fieldNums ->
            when List.get fieldNums fieldNumIdx is
                Ok fieldNum ->
                    if fieldNumMatches notes ticketFieldNum fieldNum then
                        newFieldNumIdx = fieldNumIdx + 1
                        processTicketField possibilities notes ticketFieldNum newFieldNumIdx
                    else
                        newPossibilities = removeFieldNumMatch possibilities ticketFieldNum fieldNum
                        processTicketField newPossibilities notes ticketFieldNum fieldNumIdx
                _ ->
                    newPossibilities = removeMatchedNum possibilities ticketFieldNum
                    newTicketFieldNum = ticketFieldNum + 1
                    processTicketField newPossibilities notes newTicketFieldNum 0
        _ ->
            possibilities


fieldNumMatches : Notes, I64, I64 -> Bool
fieldNumMatches = \notes, ticketFieldNum, fieldNum ->
    when List.get notes.fields fieldNum is
        Ok field ->
            fieldMatchesAllTickets field ticketFieldNum notes.nearby 0
        _ ->
            False


fieldMatchesAllTickets : Field, I64, List Ticket, I64 -> Bool
fieldMatchesAllTickets = \field, ticketFieldNum, tickets, idx ->
    when List.get tickets idx is
        Ok ticket ->
            when List.get ticket ticketFieldNum is
                Ok value ->
                    if valueMatchesField value field then
                        newIdx = idx + 1
                        fieldMatchesAllTickets field ticketFieldNum tickets newIdx
                    else
                        False
                _ ->
                    False
        _ ->
            True


removeFieldNumMatch : Possibilities, I64, I64 -> Possibilities
removeFieldNumMatch = \possibilities, ticketFieldNum, fieldNum ->
    when List.get possibilities.nums ticketFieldNum is
        Ok fieldNums ->
            newFieldNums = removeFieldNum fieldNums fieldNum 0 []
            newNums = List.set possibilities.nums ticketFieldNum newFieldNums
            newPossibilities = { possibilities & nums: newNums }
            decrementFieldCnt possibilities ticketFieldNum fieldNum
        _ ->
            possibilities


removeFieldNum : List I64, I64, I64, List I64 -> List I64
removeFieldNum = \nums, fieldNum, idx, result ->
    when List.get nums idx is
        Ok n ->
            newIdx = idx + 1
            newResult =
                if n == fieldNum then
                    result
                else
                    List.append result n
            removeFieldNum nums fieldNum newIdx newResult
        _ ->
            result


decrementFieldCnt : Possibilities, I64, I64 -> Possibilities
decrementFieldCnt = \possibilities, ticketFieldNum, fieldNum ->
    when List.get cnts fieldNum is
        Ok n ->
            newCnts = List.set cnts fieldNum (n - 1)
            newPossibilities = { possibilities & cnts: newCnts }
            if n == 2 then
                removeOtherFieldNums newPossibilities ticketFieldNum fieldNum 0
            else
                newPossibilities
        _ ->
            possibilities


removeOtherFieldNums : Possibilities, I64, I64, I64 -> Possibilities
removeOtherFieldNums = \possibilities, ticketFieldNum, fieldNum, idx ->
    when List.get possibilities.nums ticketFieldNum is
        Ok fieldNums ->
            when List.get fieldNums idx is
                Ok n ->
                    newIdx = idx + 1
                    if n == fieldNum then
                        removeOtherFieldNums possibilities ticketFieldNum fieldNum newIdx
                    else
                        newPossibilities = removeFieldNumMatch possibilities ticketFieldNum n
                        removeOtherFieldNums newPossibilities ticketFieldNum fieldNum newIdx
                _ ->
                    newNums = [ fieldNum ]
                    { possibilities & nums: newNums }
        _ ->
            possibilities


removeMatchedNum : Possibilities, I64 -> Possibilities
removeMatchedNum = \possibilities, ticketFieldNum ->
    when List.get possibilities.nums ticketFieldNum is
        Ok fieldNums ->
            if List.len fieldNums == 1 then
                when List.get fieldNums 0 is
                    Ok fieldNum ->
                        removeMatchedFieldNum possibilities ticketFieldNum fieldNum 0
                    _ ->
                        possibilities
            else
                possibilities
        _ ->
            possibilities


removeMatchedFieldNum : Possibilities, I64, I64, I64 -> Possibilities
removeMatchedFieldNum = \possibilities, ticketFieldNum, fieldNum, idx ->
    when List.get possibilities.nums idx is
        Ok fieldNums ->
            newPossibilities =
                if idx == ticketFieldNum then
                    possibilities
                else if contains fieldNums fieldNum 0 then
                    removeFieldNumMatch possibilities idx fieldNum
            newIdx = idx + 1
            removeMatchedFieldNum newPossibilities ticketFieldNum fieldNum newIdx
        _ ->
            possibilities


#  utils


sumOfInvalidTicketValues : Ticket, List Field, I64, I64 -> I64
sumOfInvalidTicketValues = \ticket, fields, idx, result ->
    when List.get ticket idx is
        Ok value ->
            newIdx = idx + 1
            newResult =
                if valueMatchesAnyField value fields 0 then
                    result
                else
                    result + value
            sumOfInvalidTicketValues ticket fields newIdx newResult
        _ ->
            result


valueMatchesAnyField : I64, List Field, I64 -> Bool
valueMatchesAnyField = \value, fields, idx ->
    when List.get fields idx is
        Ok field ->
            if valueMatchesField value field then
                True
            else
                newIdx = idx + 1
                valueMatchesAnyField value fields newIdx
        _ ->
            False


valueMatchesField : I64, Field -> Bool
valueMatchesField = \value, field ->
    (field.min1 <= value && value <= field.max1) || (field.min2 <= value && value <= field.max2)


#  parser


parseData : List I64 -> Notes
parseData = \input ->
    zip = ListZip.newAtFirst input 0

    fields = parseFieldList zip input
    mine = parseTicket fields.zip input
    nearby = parseTicketList mine.zip input

    { fields: fields.val, mine: mine.val, nearby: nearby.val }


Res a : { zip : ListZip.Zip, val : a }


parseFieldList : ListZip.Zip, List I64 -> Res (List Field)
parseFieldList = \zip, input ->
    parseFieldListHelper zip input []


parseFieldListHelper : ListZip.Zip, List I64, List Field -> Res (List Field)
parseFieldListHelper = \zip, input, result ->
    if zip.val == 10 then
        { zip, val: result }
    else
        idx = List.len result
        res = parseField zip input idx
        newResult = List.join [ result, [ res.val ] ]
        parseFieldListHelper res.zip input newResult


parseField : ListZip.Zip, List I64, I64 -> Res Field
parseField = \zip, input, idx ->
    min1 = parseInt zip input
    max1 = parseInt min1.zip input
    min2 = parseInt max1.zip input
    max2 = parseInt min2.zip input
    eol = ListZip.forward max2.zip input

    field = { idx, min1: min1.val, max1: max1.val, min2: min2.val, max2: max2.val }
    { zip: eol, val: field }


parseTicketList : ListZip.Zip, List I64 -> Res (List Ticket)
parseTicketList = \zip, input ->
    parseTicketListHelper zip input []


parseTicketListHelper : ListZip.Zip, List I64, List Ticket -> Res (List Ticket)
parseTicketListHelper = \zip, input, result ->
    if ListZip.afterLast zip then
        { zip, val: result }
    else
        res = parseTicket zip input
        newResult = List.join [ result, [ res.val ] ]
        parseTicketListHelper res.zip input newResult


parseTicket : ListZip.Zip, List I64 -> Res Ticket
parseTicket = \zip, input ->
    newZip = moveToDigit zip input
    parseTicketHelper newZip input []


parseTicketHelper : ListZip.Zip, List I64, List I64 -> Res Ticket
parseTicketHelper = \zip, input, result ->
    if zip.val == 10 || ListZip.afterLast zip then
        { zip, val: result }
    else
        res = parseInt zip input
        newResult = List.append result res.val
        parseTicketHelper res.zip input newResult


parseInt : ListZip.Zip, List I64 -> Res I64
parseInt = \zip, input ->
    digit = moveToDigit zip input
    parseIntHelper digit input 0


parseIntHelper : ListZip.Zip, List I64, I64 -> Res I64
parseIntHelper = \zip, input, num ->
    newZip = ListZip.forward zip input
    newNum = num * 10 + zip.val - 48
    if 48 <= newZip.val && newZip.val <= 57 then
        parseIntHelper newZip input newNum
    else
        { zip: newZip, val: newNum }


moveToDigit : ListZip.Zip, List I64 -> ListZip.Zip
moveToDigit = \zip, input ->
    if 48 <= zip.val && zip.val <= 57 then
        zip
    else
        newZip = ListZip.forward zip input
        moveToDigit newZip input


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
    [ 99, 108, 97, 115, 115, 58, 32, 49, 45, 49, 32, 111, 114, 32, 52, 45, 49, 57, 10
    , 114, 111, 119, 58, 32, 49, 45, 53, 32, 111, 114, 32, 56, 45, 49, 57, 10
    , 115, 101, 97, 116, 58, 32, 49, 45, 49, 51, 32, 111, 114, 32, 49, 54, 45, 49, 57, 10
    , 10
    , 121, 111, 117, 114, 32, 116, 105, 99, 107, 101, 116, 58, 10
    , 49, 49, 44, 49, 50, 44, 49, 51, 10
    , 10
    , 110, 101, 97, 114, 98, 121, 32, 116, 105, 99, 107, 101, 116, 115, 58, 10
    , 51, 44, 57, 44, 49, 56, 10
    , 49, 53, 44, 49, 44, 53, 10
    , 53, 44, 49, 52, 44, 57
    ]
