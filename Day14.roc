interface Day14 exposes [ output ] imports [ ListSet, ListZip, TestUtil ]


output : List I64 -> List (List I64)
output = \puzzleInput ->
    testData1  = parseData testInput1
    testData2  = parseData testInput2
    puzzleData = parseData puzzleInput

    [ TestUtil.verify 14 1 1 (firstResult testData1 ) 165
    , TestUtil.show   14 1   (firstResult puzzleData)
    , TestUtil.verify 14 2 1 (secondResult testData2 ) 208
    , TestUtil.show   14 2   (secondResult puzzleData)
    ]


Instruction : { cmd: I64, num1: I64, num2: I64 }


#  first part


firstResult : List Instruction -> I64
firstResult = \data ->
    initial = { maskOr: 0, maskAnd: 0, memory: [] }
    final = List.walk data instWalker1 initial
    memorySum final.memory 0 1


State : { maskOr : I64, maskAnd : I64, memory : List I64 }


instWalker1 : Instruction, State -> State
instWalker1 = \inst, state ->
    if inst.cmd == 97 then
        { state & maskOr: inst.num1, maskAnd: inst.num2 }
    else
        newVal = inst.num2 |> bitOr state.maskOr |> bitAnd state.maskAnd
        newMemory = setMemory state.memory inst.num1 newVal
        { state & memory: newMemory }


setMemory : List I64, I64, I64 -> List I64
setMemory = \memory, adr, val ->
    idx = memoryIdx memory adr 0

    ensMemory =
        if idx > 0 then
            memory
        else
            memory |> List.append adr |> List.append 0
    
    ensIdx =
        if idx > 0 then
            idx
        else
            List.len ensMemory - 1
    
    List.set ensMemory ensIdx val


memoryIdx : List I64, I64, I64 -> I64
memoryIdx = \memory, adr, idx ->
    when List.get memory idx is
        Ok val ->
            if val == adr then
                idx + 1
            else
                newIdx = idx + 2
                memoryIdx memory adr newIdx
        _ ->
            0


memorySum : List I64, I64, I64 -> I64
memorySum = \memory, result, idx ->
    when List.get memory idx is
        Ok val ->
            newResult = result + val
            newIdx = idx + 2
            memorySum memory newResult newIdx
        _ ->
            result


#  second part


secondResult : List Instruction -> I64
secondResult = \data ->
    initialSet = ListSet.emptyWithConfig 1000
    lastIdx = List.len data
    adrs = []
    adr = ListZip.newAtFirst adrs 0
    secondHelper data initialSet 0 0 0 lastIdx (lastIdx + 1) adrs adr 0


#  0,   0,  0,  0, 4, 5, 0, 0,  0,   0
#  0,   0,  0, 11, 2, 4, 0, 0,  0,   0
#  0,   0,  0, 11, 2, 3, 8, 0, 16,   1
#  1,   1,  0, 11, 2, 3, 8, 1, 17,   1
#  2,   2,  0, 11, 2, 3, 8, 2, 18,   1
#  3,   3,  0, 11, 2, 3, 8, 3, 19,   1
#  4,   4,  0, 11, 2, 3, 8, 4, 24,   1
#  5,   5,  0, 11, 2, 3, 8, 5, 25,   1
#  6,   6,  0, 11, 2, 3, 8, 6, 26,   1
#  7,   7,  0, 11, 2, 3, 8, 7, 27,   1
#  8,   8,  0, 11, 2, 3, 8, 8,  0,   1
#  8,   8, 18, 51, 0, 2, 8, 8,  0,   1
#  8,   8, 18, 51, 0, 1, 4, 0, 26, 100
#  8,   8, 18, 51, 0, 1, 4, 1, 27, 100
#  8,   8, 18, 51, 0, 1, 4, 2, 58, 100
#  9, 108, 18, 51, 0, 1, 4, 3, 59, 100
# 10, 208, 18, 51, 0, 1, 4, 4,  0, 100


secondHelper : List Instruction, List I64, I64, I64, I64, I64, I64, List I64, ListZip.Zip, I64 -> I64
secondHelper = \data, set, sum, maskOr, maskAnd, idxMask, idxMem, adrs, adr, val ->
    if ListZip.afterLast adr then
        newIdxMem = idxMem - 1
        if newIdxMem > idxMask then
            when List.get data newIdxMem is
                Ok inst ->
                    newAdrs = addresses maskOr maskAnd inst.num1
                    newAdr = ListZip.newAtFirst newAdrs 0
                    newVal = inst.num2
                    secondHelper data set sum maskOr maskAnd idxMask newIdxMem newAdrs newAdr newVal
                _ ->
                    sum
        else
            newIdxMask = nextIdxMask data idxMask
            if newIdxMask < 0 then
                sum
            else
                when List.get data newIdxMask is
                    Ok inst ->
                        newMaskOr = inst.num1
                        newMaskAnd = inst.num2
                        secondHelper data set sum newMaskOr newMaskAnd newIdxMask newIdxMem adrs adr val
                    _ ->
                        sum
    else
        newSet = ListSet.insert set adr.val
        newSum =
            if ListSet.inserted newSet then
                sum + val
            else
                sum
        newAdr = ListZip.forward adr adrs
        secondHelper data newSet newSum maskOr maskAnd idxMask idxMem adrs newAdr val


nextIdxMask : List Instruction, I64 -> I64
nextIdxMask = \data, idxMask ->
    newIdxMask = idxMask - 1
    if newIdxMask < 0 then
        newIdxMask
    else
        when List.get data newIdxMask is
            Ok inst ->
                if inst.cmd == 97 then
                    newIdxMask
                else
                    nextIdxMask data newIdxMask
            _ ->
                newIdxMask


addresses : I64, I64, I64 -> List I64
addresses = \one, zero, adr ->
    zip = ListZip.newAtFirst bits 0
    addressesHelper zip one zero adr [ 0 ]


addressesHelper : ListZip.Zip, I64, I64, I64, List I64 -> List I64
addressesHelper = \zip, one, zero, adr, result ->
    if ListZip.afterLast zip then
        result
    else
        newZip = ListZip.forward zip bits

        if bitGet one zip.val > 0 then
            newResult = addAddressBit result zip.val
            addressesHelper newZip one zero adr newResult
        
        else if bitGet zero zip.val > 0 then
            resultWithBitClr = result
            resultWithBitSet = addAddressBit result zip.val
            newResult = List.concat resultWithBitClr resultWithBitSet

            addressesHelper newZip one zero adr newResult

        else
            newResult =
                if bitGet adr zip.val > 0 then
                    addAddressBit result zip.val
                else
                    result
            addressesHelper newZip one zero adr newResult


addAddressBit : List I64, I64 -> List I64
addAddressBit = \result, bit ->
    List.map result (\n -> n + bit)


#setMemory2 : List I64, I64, I64 -> List I64
#setMemory2 = \memory, adr, val ->
#    ListTree.insert memory adr val


#memorySum2 : List I64 -> I64
#memorySum2 = \memory ->
#    ListTree.toList memory |> List.sum


#  bit operations


bitAnd : I64, I64 -> I64
bitAnd = \num1, num2 ->
    zip = ListZip.newAtFirst bits 0
    bitAndHelper zip num1 num2


bitAndHelper : ListZip.Zip, I64, I64 -> I64
bitAndHelper = \zip, num1, num2 ->
    if ListZip.afterLast zip then
        num1
    else
        newZip = ListZip.forward zip bits
        newNum1 =
            if bitGet num2 zip.val > 0 then
                num1
            else
                bitClr num1 zip.val
        bitAndHelper newZip newNum1 num2


bitOr : I64, I64 -> I64
bitOr = \num1, num2 ->
    zip = ListZip.newAtFirst bits 0
    bitOrHelper zip num1 num2


bitOrHelper : ListZip.Zip, I64, I64 -> I64
bitOrHelper = \zip, num1, num2 ->
    if ListZip.afterLast zip then
        num1
    else
        newZip = ListZip.forward zip bits
        newNum1 =
            if bitGet num2 zip.val > 0 then
                bitSet num1 zip.val
            else
                num1
        bitOrHelper newZip newNum1 num2


bitGet : I64, I64 -> I64
bitGet = \num, bit ->
    when num // bit is
        Ok shifted ->
            when shifted % 2 is
                Ok bitVal -> bitVal
                _ -> -1
        _ -> -1


bitSet : I64, I64 -> I64
bitSet = \num, bit ->
    if bitGet num bit > 0 then
        num
    else
        num + bit


bitClr : I64, I64 -> I64
bitClr = \num, bit ->
        if bitGet num bit > 0 then
            num - bit
        else
            num


bits : List I64
bits =
    [ 1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024, 2048, 4096, 8192, 16384
    , 32768, 65536, 131072, 262144, 524288, 1048576, 2097152, 4194304, 8388608
    , 16777216, 33554432, 67108864, 134217728, 268435456, 536870912, 1073741824
    , 2147483648, 4294967296, 8589934592, 17179869184, 34359738368
    ]


#  test data


parseData : List I64 -> List Instruction
parseData = \input ->
    zip = ListZip.newAt input 1 65
    parseHelper zip input 0 0 0 []


parseHelper : ListZip.Zip, List I64, I64, I64, I64, List Instruction -> List Instruction
parseHelper = \zip, input, cmd, num1, num2, result ->
    if ListZip.afterLast zip then
        inst = { cmd, num1, num2 }
        newResult = List.join [ result, [ inst ] ]
        newResult
    else if zip.val == 10 then
        newZip = ListZip.move zip input 2
        inst = { cmd, num1, num2 }
        newResult = List.join [ result, [ inst ] ]
        parseHelper newZip input 0 0 0 newResult
    else if cmd == 0 then
        newCmd = zip.val
        if newCmd == 97 then
            newZip = ListZip.move zip input 6
            parseHelper newZip input newCmd 0 0 result
        else
            newZip = ListZip.move zip input 3
            parseHelper newZip input newCmd 0 0 result
    else if cmd == 97 then
        newZip = ListZip.forward zip input
        newNum1 = 2 * num1 + (if zip.val == 49 then 1 else 0)
        newNum2 = 2 * num2 + (if zip.val == 48 then 0 else 1)
        parseHelper newZip input cmd newNum1 newNum2 result
    else if cmd == 102 then
        newZip = ListZip.forward zip input
        newNum2 = 10 * num2 + zip.val - 48
        parseHelper newZip input cmd num1 newNum2 result
    else if zip.val == 93 then
        newZip = ListZip.move zip input 4
        newCmd = 102
        parseHelper newZip input newCmd num1 num2 result
    else
        newZip = ListZip.forward zip input
        newNum1 = 10 * num1 + zip.val - 48
        parseHelper newZip input cmd newNum1 num2 result


testInput1 : List I64
testInput1 =
    [ 109, 97, 115, 107, 32, 61, 32, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 49, 88, 88, 88, 88, 48, 88, 10
    , 109, 101, 109, 91, 56, 93, 32, 61, 32, 49, 49, 10
    , 109, 101, 109, 91, 55, 93, 32, 61, 32, 49, 48, 49, 10
    , 109, 101, 109, 91, 56, 93, 32, 61, 32, 48
    ]


testInput2 : List I64
testInput2 =
    [ 109, 97, 115, 107, 32, 61, 32, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 88, 49, 48, 48, 49, 88, 10
    , 109, 101, 109, 91, 52, 50, 93, 32, 61, 32, 49, 48, 48, 10
    , 109, 97, 115, 107, 32, 61, 32, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 88, 48, 88, 88, 10
    , 109, 101, 109, 91, 50, 54, 93, 32, 61, 32, 49
    ]
