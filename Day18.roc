interface Day18 exposes [ output ] imports [ ListZip, TestUtil ]


output : List I64 -> List (List I64)
output = \puzzleInput ->
    testData1  = parseData testInput1
    testData2  = parseData testInput2
    testData3  = parseData testInput3
    testData4  = parseData testInput4
    testData5  = parseData testInput5
    testData6  = parseData testInput6
    puzzleData = parseData puzzleInput

    [ TestUtil.verify 18 1 1 (firstResult testData1 ) 71
    , TestUtil.verify 18 1 2 (firstResult testData2 ) 51
    , TestUtil.verify 18 1 3 (firstResult testData3 ) 26
    , TestUtil.verify 18 1 4 (firstResult testData4 ) 437
    , TestUtil.verify 18 1 5 (firstResult testData5 ) 12240
    , TestUtil.verify 18 1 6 (firstResult testData6 ) 13632
    , TestUtil.show   18 1   (firstResult puzzleData)
    , TestUtil.verify 18 2 1 (secondResult testData1 ) 231
    , TestUtil.verify 18 2 2 (secondResult testData2 ) 51
    , TestUtil.verify 18 2 3 (secondResult testData3 ) 46
    , TestUtil.verify 18 2 4 (secondResult testData4 ) 1445
    , TestUtil.verify 18 2 5 (secondResult testData5 ) 669060
    , TestUtil.verify 18 2 6 (secondResult testData6 ) 23340
    , TestUtil.show   18 2   (secondResult puzzleData)
    ]


#  first part


firstResult : List (List I64) -> I64
firstResult = \lines ->
    List.map lines eval |> List.sum


eval : List I64 -> I64
eval = \input ->
    if List.len input > 0 then
        zip = ListZip.newAtFirst input 0
        evalHelper input zip [] -1
    else
        0


#        | (  ->  (
#        | +  ->  +
#        | *  ->  *
#      ( | n  ->  n
#    m + | n  ->  (m+n)
#    m * | n  ->  (m*n)
#        | n  ->  n
#  m + n | )  ->  (m+n)
#  m * n | )  ->  (m*n)
#      n | )  ->  n

evalHelper : List I64, ListZip.Zip, List I64, I64 -> I64
evalHelper = \input, zip, stack, sp ->
    newZip = ListZip.forward zip input
    if zip.val < 0 then
        newSp = sp + 1
        newStack = setStack stack newSp zip.val
        evalHelper input newZip newStack newSp
    else
        val =
            if zip.val > 0 then
                zip.val
            else
                getStack stack sp
        opSp =
            if zip.val > 0 then
                sp
            else
                sp - 1
        if opSp < 0 then
            val
        else
            when getStack stack opSp is
                -2 ->
                    newSp = opSp - 1
                    newStack = setStack stack newSp (val * (getStack stack newSp))
                    evalHelper input newZip newStack newSp
                -3 ->
                    newSp = opSp - 1
                    newStack = setStack stack newSp (val + (getStack stack newSp))
                    evalHelper input newZip newStack newSp
                _ ->
                    newStack = setStack stack opSp val
                    evalHelper input newZip newStack opSp


#  second part


secondResult : List (List I64) -> I64
secondResult = \lines ->
    List.map lines eval2 |> List.sum


eval2 : List I64 -> I64
eval2 = \input ->
    if List.len input > 0 then
        zip = ListZip.newAtFirst input 0
        eval2Helper input zip [] -1
    else
        0


#              | (  ->  (
#              | +  ->  +
#        m * n | *  ->  (m*n) *
#              | *  ->  *
#          m + | n  ->  (m+n)
#              | n  ->  n
#  o + ( m * n | )  ->  (o+(m*n))
#      m + ( n | )  ->  (m+n)
#      ( m * n | )  ->  (m*n)
#          ( n | )  ->  n

eval2Helper : List I64, ListZip.Zip, List I64, I64 -> I64
eval2Helper = \input, zip, stack, sp ->
    newZip = ListZip.forward zip input

    if zip.val == -2 then
        opSp = sp - 1
        if getStack stack opSp == -2 then
            resSp = opSp - 1
            newStack = setStack stack resSp (getStack stack resSp * getStack stack sp)
            eval2Helper input newZip newStack opSp
        else
            newSp = sp + 1
            newStack = setStack stack newSp zip.val
            eval2Helper input newZip newStack newSp

    else if zip.val < 0 then
        newSp = sp + 1
        newStack = setStack stack newSp zip.val
        eval2Helper input newZip newStack newSp

    else if zip.val > 0 then
        if getStack stack sp == -3 then
            newSp = sp - 1
            newStack = setStack stack newSp (zip.val + (getStack stack newSp))
            eval2Helper input newZip newStack newSp
        else
            newSp = sp + 1
            newStack = setStack stack newSp zip.val
            eval2Helper input newZip newStack newSp

    else
        val1 = getStack stack sp
        opSp1 = sp - 1

        val2 =
            if getStack stack opSp1 == -2 then
                val1 * getStack stack (opSp1 - 1)
            else
                val1
        opSp2 =
            if getStack stack opSp1 == -2 then
                opSp1 - 3
            else
                opSp1 - 1

        if opSp2 < 0 then
            val2
        else if getStack stack opSp2 == -3 then
            newSp = opSp2 - 1
            newStack = setStack stack newSp (val2 + (getStack stack newSp))
            eval2Helper input newZip newStack newSp
        else
            newSp = opSp2 + 1
            newStack = setStack stack newSp val2
            eval2Helper input newZip newStack newSp


#  utils


getStack : List I64, I64 -> I64
getStack = \stack, idx ->
    when List.get stack idx is
        Ok val -> val
        _ -> 0


setStack : List I64, I64, I64 -> List I64
setStack = \stack, idx, val ->
    if idx < List.len stack then
        List.set stack idx val
    else
        setStack (List.append stack 0) idx val


#  parser


parseData : List I64 -> List (List I64)
parseData = \input ->
    parseDataHelper input (ListZip.newAtFirst input 0) 0 [ -1 ] [ [] ]


parseDataHelper : List I64, ListZip.Zip, I64, List I64, List (List I64) -> List (List I64)
parseDataHelper = \input, zip, num, line, result ->
    if ListZip.afterLast zip then
        newLine =
            if num > 0 then
                List.append line num
            else
                line
        List.join [ result, [ List.append newLine 0 ] ]
    else
        newZip = ListZip.forward zip input
        if 48 <= zip.val && zip.val <= 57 then
            parseDataHelper input newZip (10 * num + zip.val - 48) line result
        else
            newLine =
                if num > 0 then
                    List.append line num
                else
                    line
            when zip.val is
                10 -> parseDataHelper input newZip 0 [ -1 ] (List.join [ result, [ List.append newLine 0 ] ])
                40 -> parseDataHelper input newZip 0 (List.append newLine -1) result
                41 -> parseDataHelper input newZip 0 (List.append newLine  0) result
                42 -> parseDataHelper input newZip 0 (List.append newLine -2) result
                43 -> parseDataHelper input newZip 0 (List.append newLine -3) result
                _ ->  parseDataHelper input newZip 0 newLine result


#  test data


testInput1 : List I64
testInput1 =
    [ 49, 32, 43, 32, 50, 32, 42, 32, 51, 32, 43, 32, 52, 32, 42, 32, 53, 32, 43
    , 32, 54
    ]


testInput2 : List I64
testInput2 =
    [ 49, 32, 43, 32, 40, 50, 32, 42, 32, 51, 41, 32, 43, 32, 40, 52, 32, 42, 32
    , 40, 53, 32, 43, 32, 54, 41, 41
    ]


testInput3 : List I64
testInput3 =
    [ 50, 32, 42, 32, 51, 32, 43, 32, 40, 52, 32, 42, 32, 53, 41 ]


testInput4 : List I64
testInput4 =
    [ 53, 32, 43, 32, 40, 56, 32, 42, 32, 51, 32, 43, 32, 57, 32, 43, 32, 51, 32
    , 42, 32, 52, 32, 42, 32, 51, 41
    ]


testInput5 : List I64
testInput5 =
    [ 53, 32, 42, 32, 57, 32, 42, 32, 40, 55, 32, 42, 32, 51, 32, 42, 32, 51, 32
    , 43, 32, 57, 32, 42, 32, 51, 32, 43, 32, 40, 56, 32, 43, 32, 54, 32, 42, 32
    , 52, 41, 41
    ]


testInput6 : List I64
testInput6 =
    [ 40, 40, 50, 32, 43, 32, 52, 32, 42, 32, 57, 41, 32, 42, 32, 40, 54, 32, 43
    , 32, 57, 32, 42, 32, 56, 32, 43, 32, 54, 41, 32, 43, 32, 54, 41, 32, 43, 32
    , 50, 32, 43, 32, 52, 32, 42, 32, 50
    ]
