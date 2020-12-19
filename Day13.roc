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


secondResult : List I64 -> I64
secondResult = \data ->
    secondResultHelper data 0 1 1


secondResultHelper : List I64, I64, I64, I64 -> I64
secondResultHelper = \data, rem, prd, idx ->
    when List.get data idx is
        Ok bus ->
            newIdx = idx + 1
            if bus > 0 then
                busRem = absM (bus - idx + 1) bus
                newRem = crt rem prd busRem bus
                newPrd = prd * bus
                secondResultHelper data newRem newPrd newIdx
            else
                secondResultHelper data rem prd newIdx
        _ ->
            rem


#  maths


Eea : { bez1 : I64, bez2 : I64, gcd : I64, quot1 : I64, quot2 : I64 }


eea : I64, I64 -> Eea
eea = \a, b ->
    eeaHelper { oldR: a, r: b, oldS: 1, s: 0, oldT: 0, t: 1 }


EeaAcc : { oldR : I64, r : I64, oldS : I64, s : I64, oldT : I64, t : I64 }


eeaHelper : EeaAcc -> Eea
eeaHelper = \acc ->
    when acc.oldR // acc.r is
        Ok quotient ->
            eeaHelper
                { oldR: acc.r, r: acc.oldR - quotient * acc.r
                , oldS: acc.s, s: acc.oldS - quotient * acc.s
                , oldT: acc.t, t: acc.oldT - quotient * acc.t
                }
        _ ->
            { bez1: acc.oldS, bez2: acc.oldT, gcd: acc.oldR, quot1: acc.t, quot2: acc.s }


crt : I64, I64, I64, I64 -> I64
crt = \a1, n1, a2, n2 ->
    e = eea n1 n2

    a1b2 = mulM a1 e.bez2 n1
    a2b1 = mulM a2 e.bez1 n2
    n1n2 = n1 * n2
    
    c = mulM a1b2 n2 n1n2 + mulM a2b1 n1 n1n2
    absM c n1n2


absM : I64, I64 -> I64
absM = \a, m ->
    when a % m is
        Ok n -> if n < 0 then n + m else n
        _ -> 0


mulM : I64, I64, I64 -> I64
mulM = \a, b, m ->
    mulMHelper (absM a m) (absM b m) m 0


mulMHelper : I64, I64, I64, I64 -> I64
mulMHelper = \a, b, m, q ->
    if b > 0 then
        newA =
            when (2 * a) % m is
                Ok n -> n
                _ -> 0
        newB =
            when b // 2 is
                Ok n -> n
                _ -> 0
        newQ =
            if 2 * newB < b then
                when (q + a) % m is
                    Ok n -> n
                    _ -> 0
            else
                q
        mulMHelper newA newB m newQ
    else
        q


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
